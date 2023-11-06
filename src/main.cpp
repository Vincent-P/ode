#define _CRT_SECURE_NO_WARNINGS
#define SOKOL_IMPL
#include "sokol_time.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cross.h"
#include "vm.h"

// options
sv src_dir_arg = {};
// modules
using NameBuffer = char[64];
NameBuffer modules_name[32];
sv modules_name_sv[32];
NameBuffer modules_path[32];
sv modules_path_sv[32];
uint64_t modules_file_last_write[32];
uint32_t modules_len;

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

static bool get_module_path(char *out_path, uint32_t *out_path_length, sv src_dir, sv module_name)
{
	// src/ module_name .ode \0
	if ((4 + module_name.length + 4 + 1) > *out_path_length) {
		return false;
	}
	// Append src dir
	uint32_t i = 0;
	uint32_t i_src_dir = 0;
	for (; i_src_dir < src_dir.length; ++i_src_dir) {
		out_path[i + i_src_dir] = src_dir.chars[i_src_dir];
	}
	i += i_src_dir;
	// Add separator
	out_path[i++] = '/';
	// Append module name
	uint32_t i_module_name = 0;
	for (; i_module_name < module_name.length; ++i_module_name) {
		out_path[i + i_module_name] = module_name.chars[i_module_name];
	}
	i += i_module_name;
	// Append '.ode'
	out_path[i++] = '.';
	out_path[i++] = 'o';
	out_path[i++] = 'd';
	out_path[i++] = 'e';
	// Terminate with 0
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
	printf("log foreign func.\n");
}

void logi_foreign_func(VM *)
{
	// StackValue n = execution_get_local(ctx, 0);
	// printf("\n=== HOST: log(%d) ===\n", n.i32);
	printf("logi foreign func.\n");
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
	// Open the module file
	char module_path[64] = {};
	uint32_t module_path_length = 64;
	bool success = get_module_path(module_path, &module_path_length, src_dir_arg, module_name);
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
	// Register the module in our modules array
	uint32_t i_module = 0;
	for (; i_module < modules_len; ++i_module) {
		if (sv_equals(modules_name_sv[i_module], module_name)) {
			break;
		}
	}
	if (i_module >= modules_len) {
		// Add a new module
		i_module = modules_len;
		modules_len += 1;
		// Init name
		memcpy(&modules_name[i_module], module_name.chars, module_name.length);
		modules_name[i_module][module_name.length] = '\0';
		modules_name_sv[i_module].chars = modules_name[i_module];
		modules_name_sv[i_module].length = module_name.length;
		// Init path
		memcpy(&modules_path[i_module], module_path, sizeof(module_path));
		modules_path_sv[i_module].chars = modules_path[i_module];
		modules_path_sv[i_module].length = module_path_length;
		// Init time
		modules_file_last_write[i_module] = 0;
	}

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
	if (argc < 3) {
		fprintf(stderr, "Usage: ode.exe <src dir> <main_module> (-w)\n");
		return 1;
	}
	// Paths
	src_dir_arg = sv_from_null_terminated(argv[1]);
	const sv main_module_arg = sv_from_null_terminated(argv[2]);
	// Options
	const sv watch_arg = sv_from_null_terminated("-w");
	bool watch_opt = false;
	for (int i_arg = 3; i_arg < argc; ++i_arg) {
		sv arg = sv_from_null_terminated(argv[i_arg]);
		if (sv_equals(arg, watch_arg)) {
			watch_opt = true;
		}
	}

	// Misc
	stm_setup();                              // Enable timer
	setvbuf(stdout, (char *)NULL, _IONBF, 0); // Disable stdout buffering

	// Setup the main module
	// Add the module as module#0
	modules_len = 1;
	// Init name
	modules_name_sv[0] = main_module_arg;
	// Init path
	modules_path_sv[0].chars = modules_path[0];
	modules_path_sv[0].length = 64; // buffer size is 64, get_module_path will replace it with real length
	if (!get_module_path(modules_path[0], &modules_path_sv[0].length, src_dir_arg, main_module_arg)) {
		return 1;
	}
	// Init time
	modules_file_last_write[0] = 0;
	
	// Create the VM
	VMConfig vm_config = {};
	vm_config.load_module = on_load_module;
	vm_config.error_callback = on_error;
	vm_config.foreign_callback = on_foreign;
	VM *vm = vm_create(vm_config);
	// Watch and execute
	bool stop = !watch_opt;
	do {
		bool any_module_changed = false;
		for (uint32_t i_module = 0; i_module < modules_len; ++i_module) {
			const uint64_t last_write = cross::get_file_last_write(modules_path_sv[i_module].chars, modules_path_sv[i_module].length);
			if (last_write != modules_file_last_write[i_module]) {
				modules_file_last_write[i_module] = last_write;
				any_module_changed = true;
				
				// read entire source code
				FILE *input_file = fopen(modules_path[i_module], "r");
				if (input_file == nullptr) {
					fprintf(stderr, "Cannot open file %.*s\n", int(modules_path_sv[i_module].length), modules_path_sv[i_module].chars);
					return 1;
				}
				sv file_content = read_entire_file(input_file);
				fclose(input_file);
				// compile
				vm_compile(vm, modules_name_sv[i_module], file_content);
			}
		}

		if (any_module_changed) {
			vm_call(vm, modules_name_sv[0], sv_from_null_terminated("main"));
		}
		
		cross::sleep_ms(33);
	} while (!stop);
	return 0;
}
