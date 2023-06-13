#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "compiler.h"
#include "cross.h"
#include "executor.h"

sv read_entire_file(FILE *file)
{
	fseek(file, 0, SEEK_END);
	long fsize = ftell(file);
	fseek(file, 0, SEEK_SET);

	char *file_content = (char *)malloc(uint64_t(fsize) + 1);
	fread(file_content, uint64_t(fsize), 1, file);
	file_content[fsize] = 0;

	return sv{file_content, uint64_t(fsize)};
}

int compile_file_to_module(Compiler *compiler, sv path, sv *out_module_name = nullptr)
{
	if (path.chars == nullptr) {
		return 1;
	}

	sv path_extension = {path.chars + path.length - 4, 4};

	const bool ends_with_ode = path.length >= 4 && sv_equals(path_extension, sv{".ode", 4});
	if (!ends_with_ode) {
		fprintf(stderr, "Input file should have extension '.ode'\n");
		return 1;
	}

	sv module_name = path_extension;
	while (module_name.chars > path.chars) {
		const char char_before = *(module_name.chars - 1);
		const bool is_path_separator = char_before == '/' || char_before == '\\';
		if (is_path_separator) {
			break;
		}

		module_name.chars -= 1;
		module_name.length += 1;
	}

	FILE *input_file = fopen(path.chars, "r");
	if (input_file == nullptr) {
		fprintf(stderr, "Cannot open file %.*s\n", int(path.length), path.chars);
		return 1;
	}

	sv file_content = read_entire_file(input_file);
	fclose(input_file);

	fprintf(stdout, "Running script: %s\n", path.chars);
	fprintf(stdout, "module: %.*s\n", int(module_name.length), module_name.chars);
	fprintf(stdout, "%s\n", file_content.chars);

	Result res = compile_module(compiler, module_name, file_content);
	if (res == Result::Ok) {
		if (out_module_name) {
			*out_module_name = module_name;
		}
	}

	return res != Result::Ok;
}

int main(int argc, const char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Usage: ode.exe <input_file>\n");
		return 1;
	}

	// Disable stdout buffering
	setvbuf(stdout, (char *)NULL, _IONBF, 0);

	const char *const path = argv[1];
	const sv path_str = sv{path, strlen(path)};

	Compiler *compiler = compiler_init();
	Image image = {};
	ExecutorState *executor = executor_init();

	uint64_t last_compilation = 0;
	sv last_compiled_module_name = {};

	while (true) {
		const uint64_t last_write = cross::get_file_last_write(path_str.chars, path_str.length);
		if (last_write != last_compilation) {
			printf("Last file write: %llu\n", last_write);
			last_compilation = last_write;
			int res = compile_file_to_module(compiler, path_str, &last_compiled_module_name);
			printf("Compilation returned: %d\n", res);
			compiler_update_image(compiler, &image);
			executor_load_image(executor, &image);
		}

		executor_execute_module_entrypoint(executor, last_compiled_module_name);

		cross::sleep_ms(5000);
	}

	return 0;
}
