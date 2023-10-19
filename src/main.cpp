#define _CRT_SECURE_NO_WARNINGS
#define SOKOL_IMPL
#include "sokol_time.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cross.h"
#include "vm.h"

static sv read_entire_file(FILE *file)
{
	fseek(file, 0, SEEK_END);
	long fsize = ftell(file);
	fseek(file, 0, SEEK_SET);

	char *file_content = (char *)malloc(uint32_t(fsize) + 1);
	fread(file_content, uint32_t(fsize), 1, file);
	file_content[fsize] = 0;

	return sv{file_content, uint32_t(fsize)};
}

static bool get_module_path(char *out_path, uint32_t *out_path_length, sv module_name)
{
	// src/ module_name .ode \0
	if ((4 + module_name.length + 4 + 1) > *out_path_length) {
		return false;
	}

	uint32_t i = 0;
	out_path[i++] = 's';
	out_path[i++] = 'r';
	out_path[i++] = 'c';
	out_path[i++] = '/';
	uint32_t i_module_name = 0;
	for (; i_module_name < module_name.length; ++i_module_name) {
		out_path[i + i_module_name] = module_name.chars[i_module_name];
	}
	i += i_module_name;
	out_path[i++] = '.';
	out_path[i++] = 'o';
	out_path[i++] = 'd';
	out_path[i++] = 'e';
	out_path[i++] = 0;
	*out_path_length = i;

	return true;
}

void dummy_foreign_func(VM *)
{
	printf("Dummy foreign func.\n");
}

void log_foreign_func(VM *)
{
	// StackValue arg0 = execution_get_local(ctx, 0);
	// sv arg0_sv = execution_get_str(ctx, arg0.str);
	// printf("\n=== HOST: log(\"%.*s\") ===\n", int(arg0_sv.length), arg0_sv.chars);
}

void logi_foreign_func(VM *)
{
	// StackValue n = execution_get_local(ctx, 0);
	// printf("\n=== HOST: log(%d) ===\n", n.i32);
}

ForeignFn on_foreign(sv module_name, sv function_name)
{
	printf("=== HOST: On foreign function: module %.*s function %.*s ===\n",
		int(module_name.length),
		module_name.chars,
		int(function_name.length),
		function_name.chars);

	if (sv_equals(function_name, sv_from_null_terminated("log"))) {
		return log_foreign_func;
	} else if (sv_equals(function_name, sv_from_null_terminated("logi"))) {
		return logi_foreign_func;
	}

	return dummy_foreign_func;
}

bool on_load_module(sv module_name, sv *out_code)
{
	char module_path[64] = {};
	uint32_t module_path_length = 64;
	bool success = get_module_path(module_path, &module_path_length, module_name);
	if (!success) {
		return false;
	}

	FILE *input_file = fopen(module_path, "r");
	if (input_file == nullptr) {
		fprintf(stderr, "Cannot open file %s\n", module_path);
		return false;
	}

	sv file_content = read_entire_file(input_file);
	fclose(input_file);

	fprintf(stdout, "Load module: %s\n", module_path);
	*out_code = file_content;

	return true;
}

void on_error(VM *, Error err)
{
	if (err.file.chars != nullptr) {
		fprintf(stderr, "%.*s:%d:0: error:  ", int(err.file.length), err.file.chars, err.line);
	}
	fprintf(stderr, "EXECUTOR FAILED at (%zu): %.*s\n", err.ip, int(err.msg.length), err.msg.chars);
}

int main(int argc, const char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Usage: ode.exe <main_module>\n");
		return 1;
	}

	const sv watch_arg = sv_from_null_terminated("-w");
	bool watch_opt = false;

	for (int i_arg = 2; i_arg < argc; ++i_arg) {
		sv arg = sv_from_null_terminated(argv[i_arg]);

		if (sv_equals(arg, watch_arg)) {
			watch_opt = true;
		}
	}

	stm_setup();

	// Disable stdout buffering
	setvbuf(stdout, (char *)NULL, _IONBF, 0);

	const char *const main_module_name = argv[1];
	const sv main_module_name_str = sv_from_null_terminated(main_module_name);

	char buffer[64] = {};
	sv main_module_path = {};
	main_module_path.chars = buffer;
	main_module_path.length = 64;
	if (!get_module_path(buffer, &main_module_path.length, main_module_name_str)) {
		return 1;
	}

	VMConfig vm_config = {};
	vm_config.load_module = on_load_module;
	vm_config.error_callback = on_error;
	vm_config.foreign_callback = on_foreign;
	VM *vm = vm_create(vm_config);

	uint64_t last_compilation = 0;

	bool stop = !watch_opt;
	do {
		const uint64_t last_write = cross::get_file_last_write(main_module_path.chars, main_module_path.length);
		if (last_write != last_compilation) {
			last_compilation = last_write;

			FILE *input_file = fopen(main_module_path.chars, "r");
			if (input_file == nullptr) {
				fprintf(stderr, "Cannot open file %.*s\n", int(main_module_path.length), main_module_path.chars);
				return 1;
			}

			sv file_content = read_entire_file(input_file);
			fclose(input_file);

			vm_interpret(vm, main_module_name_str, file_content);
		}

		cross::sleep_ms(33);
	} while (!stop);

	return 0;
}
