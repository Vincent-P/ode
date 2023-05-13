#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "compiler.h"

char *read_entire_file(FILE *file, long *out_size)
{
	fseek(file, 0, SEEK_END);
	long fsize = ftell(file);
	fseek(file, 0, SEEK_SET);

	char *file_content = (char *)malloc(uint64_t(fsize) + 1);
	fread(file_content, uint64_t(fsize), 1, file);
	file_content[fsize] = 0;

	if (out_size)
		*out_size = fsize;

	return file_content;
}

int load_file(const char *file_path, uint64_t path_length)
{
	if (file_path == nullptr) {
		return 1;
	}

	const char *path_extension = file_path + path_length - 4;
	const bool ends_with_ode = path_length >= 4 && string_equals(path_extension, 4, ".ode", 4);
	if (!ends_with_ode) {
		fprintf(stderr, "Input file should have extension '.ode'\n");
		return 1;
	}

	const char *module_name = path_extension;
	uint64_t module_name_length = 0;

	while (module_name > file_path) {
		const char char_before = *(module_name - 1);
		const bool is_path_separator = char_before == '/' || char_before == '\\';
		if (is_path_separator) {
			break;
		}

		module_name -= 1;
		module_name_length += 1;
	}

	FILE *input_file = fopen(file_path, "r");
	if (input_file == nullptr) {
		fprintf(stderr, "Cannot open file %.*s\n", int(path_length), file_path);
		return 1;
	}

	long file_size = 0;
	char *file_content = read_entire_file(input_file, &file_size);
	fclose(input_file);

	fprintf(stdout, "Running script: %s\n", file_path);
	fprintf(stdout, "module: %.*s\n", int(module_name_length), module_name);
	fprintf(stdout, "%s\n", file_content);

	Compiler *compiler = compiler_init();
	Result res = compile_module(compiler, module_name, module_name_length, file_content, uint64_t(file_size));
	return res != Result::Ok;
}

int main(int argc, const char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Usage: ode.exe <input_file>\n");
		return 1;
	}

	const char *path = argv[1];
	uint64_t path_length = strlen(path);
	int res = load_file(path, path_length);
	return res;
}
