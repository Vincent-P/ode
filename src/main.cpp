#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "compiler.h"

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

int load_file(sv path)
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

	Compiler *compiler = compiler_init();
	Result res = compile_module(compiler, module_name, file_content);
	return res != Result::Ok;
}

int main(int argc, const char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Usage: ode.exe <input_file>\n");
		return 1;
	}

	const char *path = argv[1];
	int res = load_file(sv{path, strlen(path)});
	return res;
}
