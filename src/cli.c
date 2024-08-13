#include "crt_stub.h"

// source files
#include "core/cross.c"
#include "debug.c"
#include "type_id.c"
#include "lexer.c"
#include "parser.c"
#include "compiler.c"
#include "vm.c"
#include "executor.c"

#include "core/cross.h"
#include "vm.h"

#define CLI_LOG 0
#define CLI_MEMORY_SIZE (4u << 20)
Arena memory_arena;

static bool on_load_module(sv module_name, sv *out_code)
{
	char pathbuf[64]	 = {0};
	char logbuf[64]	 = {0};

	StringBuilder sb = string_builder_from_buffer(pathbuf, sizeof(pathbuf));
	string_builder_append_sv(&sb, module_name);
	string_builder_append_sv(&sb, SV(".ode"));
	sv path = string_builder_get_string(&sb);

	ReadFileResult file_content = cross_read_entire_file(path);
	if (!file_content.success) {
#if CLI_LOG >= 1
		sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		string_builder_append_sv(&sb, SV("Cannot open file "));
		string_builder_append_sv(&sb, path);
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));
#endif
		return false;
	}
#if CLI_LOG >= 1
	sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
	string_builder_append_sv(&sb, SV("[CLI] Load: "));
	string_builder_append_sv(&sb, path);
	string_builder_append_char(&sb, '\n');
	cross_log(cross_stderr, string_builder_get_string(&sb));
#endif

	*out_code = file_content.content;

	return true;
}

static void on_error(VM *vm, Error err)
{

}

static void cli_parse_module_rec(Arena temp_mem, VM *vm, sv module_name, int depth)
{
		ParseModuleResult parse_result = vm_parse_module(&temp_mem, vm, module_name);
		if (parse_result.result != ParseModule_Ok) {
			return;
		}

		if (depth == 0)
		{
			cross_log(cross_stdout, module_name);
			cross_log(cross_stdout, SV(".ode : "));
		}
		else
		{
			cross_log(cross_stdout, module_name);
			cross_log(cross_stdout, SV(".ode "));
		}

		for (uint32_t i_dep = 0; i_dep < parse_result.dependency_count; ++i_dep) {
			sv dependency_name = vm_get_identifier(vm, parse_result.dependencies[i_dep]);
			cli_parse_module_rec(temp_mem, vm, dependency_name, depth + 1);
		}
}

static uint32_t entrypoint(sv *args, uint32_t arg_count)
{
	cross_log(cross_stderr, SV("exe: ")); cross_log(cross_stderr, args[0]); cross_log(cross_stderr, SV("\n"));

	if (arg_count != 2)
	{
		cross_log(cross_stderr, SV("usage: ode <input_file>"));
		return 1;
	}

	sv input_module = args[1];
	cross_log(cross_stderr, SV("input module: "));
	cross_log(cross_stderr, input_module);
	cross_log(cross_stderr, SV("\n"));

	VMConfig vm_config = {0};
	vm_config.load_module = on_load_module;
	vm_config.error_callback = on_error;
	VM *vm = vm_create(&memory_arena, vm_config);

	{
		Arena temp_mem = memory_arena;
		cli_parse_module_rec(temp_mem, vm, input_module, 0);
		cross_log(cross_stdout, SV("\n"));
	}

	vm_destroy(vm);
	
	return 0;
}


#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Weverything"
#include "win32_cmdline.c"
#pragma clang diagnostic pop
int mainCRTStartup(void);
int mainCRTStartup(void)
{
	static byte memory[CLI_MEMORY_SIZE];
    static char *argv[CMDLINE_ARGV_MAX];

	memory_arena.begin = memory;
	memory_arena.end = memory + CLI_MEMORY_SIZE;
	cross_init();

	// Parse cli
    unsigned short *cmd = cmdline_fetch();
    uint32_t arg_count = (uint32_t)cmdline_to_argv8(cmd, argv);
	sv *args = arena_alloc(&memory_arena, arg_count * sizeof(sv));
	for (uint32_t i_arg = 0; i_arg < arg_count; ++i_arg)
	{
		args[i_arg] = sv_from_null_terminated(argv[i_arg]);
	}

	uint32_t exit_code = entrypoint(args, arg_count);

	ExitProcess(exit_code);
	return (int)exit_code;
}
