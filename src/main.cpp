#define _CRT_SECURE_NO_WARNINGS
#define SOKOL_IMPL
#include "sokol_time.h"

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

Module *compile_file_to_module(Compiler *compiler, sv path, sv *out_module_name = nullptr)
{
	if (path.chars == nullptr) {
		return nullptr;
	}

	sv path_extension = {path.chars + path.length - 4, 4};

	const bool ends_with_ode = path.length >= 4 && sv_equals(path_extension, sv{".ode", 4});
	if (!ends_with_ode) {
		fprintf(stderr, "Input file should have extension '.ode'\n");
		return nullptr;
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
		return nullptr;
	}

	sv file_content = read_entire_file(input_file);
	fclose(input_file);

	fprintf(stdout, "Running script: %s\n", path.chars);
	fprintf(stdout, "module: %.*s\n", int(module_name.length), module_name.chars);
	fprintf(stdout, "%s\n", file_content.chars);

	Module *module = nullptr;
	Result res = compile_module(compiler, module_name, file_content, &module);
	if (res == Result::Ok) {
		if (out_module_name) {
			*out_module_name = module_name;
		}
	}

	return module;
}

void dummy_foreign_func(ExecutionContext *)
{
	printf("Dummy foreign func.\n");
}

void log_foreign_func(ExecutionContext *ctx)
{
	StackValue arg0 = execution_get_local(ctx, 0);
	sv arg0_sv = execution_get_str(ctx, arg0.str);
	printf("\n=== HOST: log(\"%.*s\") ===\n", int(arg0_sv.length), arg0_sv.chars);
}

void logi_foreign_func(ExecutionContext *ctx)
{
	StackValue n = execution_get_local(ctx, 0);
	printf("\n=== HOST: log(%d) ===\n", n.i32);
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

void on_error(ExecutorState * /*executor*/, RuntimeError err)
{
	if (err.file.chars != nullptr) {
		fprintf(stderr, "%.*s:%d:0: error:  ", int(err.file.length), err.file.chars, err.line);
	}
	fprintf(stderr, "EXECUTOR FAILED: %.*s\n", int(err.message.length), err.message.chars);
}


int main(int argc, const char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Usage: ode.exe <input_file>\n");
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

	const char *const path = argv[1];
	const sv path_str = sv{path, strlen(path)};

	Compiler *compiler = compiler_init();


	ExecutorConfig exec_config = {};
	exec_config.error_callback = on_error;
	exec_config.foreign_callback = on_foreign;

	ExecutorState *executor = executor_init(exec_config);


	uint64_t last_compilation = 0;
	sv last_compiled_module_name = {};

	bool stop = !watch_opt;
	do {
		const uint64_t last_write = cross::get_file_last_write(path_str.chars, path_str.length);
		if (last_write != last_compilation) {
			printf("Last file write: %llu\n", last_write);
			last_compilation = last_write;
			Module *module = compile_file_to_module(compiler, path_str, &last_compiled_module_name);
			printf("Compilation returned: %p\n", static_cast<void *>(module));
			if (module != nullptr) {

				executor_load_module(executor, module);

				uint64_t before = stm_now();
				executor_execute_module_entrypoint(executor, last_compiled_module_name);
				uint64_t after = stm_now();
				uint64_t delta = stm_diff(after, before);
				double delta_ms = stm_ms(delta);
				printf("Execution lasted %f ms.\n", delta_ms);

			}
		}

		cross::sleep_ms(33);
	} while (!stop);

	return 0;
}
