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

#define CLI_LOG 1
#define CLI_MEMORY_SIZE (4u << 20)
Arena memory_arena;
sv base_dir;

static bool on_load_module(sv module_name, sv *out_code)
{
	char pathbuf[256]	 = {0};
	char logbuf[256]	 = {0};

	StringBuilder sb = string_builder_from_buffer(pathbuf, sizeof(pathbuf));
	string_builder_append_sv(&sb, base_dir);
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

static bool cli_parse_module_rec(Arena temp_mem, VM *vm, sv module_name, int depth, StringBuilder *out_sb)
{
	ParseModuleResult parse_result = vm_parse_module(&temp_mem, vm, module_name);
	if (parse_result.result != ParseModule_Ok) {
		return false;
	}

	for (uint32_t i_dep = 0; i_dep < parse_result.dependency_count; ++i_dep) {
		sv dependency_name = vm_get_identifier(vm, parse_result.dependencies[i_dep]);
		string_builder_append_char(out_sb, ' ');
		string_builder_append_sv(out_sb, base_dir);
		string_builder_append_sv(out_sb, dependency_name);
		string_builder_append_sv(out_sb, SV(".ode"));
	}
	cross_log(cross_stdout, SV("\n"));

	for (uint32_t i_dep = 0; i_dep < parse_result.dependency_count; ++i_dep) {
		sv dependency_name = vm_get_identifier(vm, parse_result.dependencies[i_dep]);
		bool dep_success = cli_parse_module_rec(temp_mem, vm, dependency_name, depth + 1, out_sb);
		if (!dep_success) {
			return false;
		}
	}

	return true;
}

static bool cli_parse_module(Arena temp_mem, VM *vm, sv module_name, StringBuilder *out_sb)
{
	ParseModuleResult parse_result = vm_parse_module(&temp_mem, vm, module_name);
	if (parse_result.result != ParseModule_Ok) {
		return false;
	}
	if (parse_result.dependency_count == 0) {
		return true;
	}
	
	string_builder_append_sv(out_sb, base_dir);
	string_builder_append_sv(out_sb, module_name);
	string_builder_append_sv(out_sb, SV(".ode :"));
	
	for (uint32_t i_dep = 0; i_dep < parse_result.dependency_count; ++i_dep) {
		sv dependency_name = vm_get_identifier(vm, parse_result.dependencies[i_dep]);
		string_builder_append_char(out_sb, ' ');
		string_builder_append_sv(out_sb, base_dir);
		string_builder_append_sv(out_sb, dependency_name);
		string_builder_append_sv(out_sb, SV(".ode"));
	}
	cross_log(cross_stdout, SV("\n"));
	
	for (uint32_t i_dep = 0; i_dep < parse_result.dependency_count; ++i_dep) {
		sv dependency_name = vm_get_identifier(vm, parse_result.dependencies[i_dep]);
		bool dep_success = cli_parse_module_rec(temp_mem, vm, dependency_name, 1, out_sb);
		if (!dep_success) {
			return false;
		}
	}

	return true;
}

static uint32_t entrypoint(sv *args, uint32_t arg_count)
{
	uint32_t result = 1;
	if (arg_count < 3)
	{
		cross_log(cross_stderr, SV("usage: ode <command> <input_module> (-d <base dir>) (-o <output_path>)\n"));
		return result;
	}

	sv command = args[1];
	sv input_module = args[2];
	sv output_path = {0};
	for (uint32_t i_arg = 3; i_arg < arg_count; ++i_arg) {
		if (sv_equals(args[i_arg], SV("-d"))) {
			if (i_arg + 1 >= arg_count) {
				cross_log(cross_stderr, SV("expected a base_dir after '-d' option flag\n"));
				return result;
			}
			base_dir = args[i_arg + 1];
		} else if (sv_equals(args[i_arg], SV("-o"))) {
			if (i_arg + 1 >= arg_count) {
				cross_log(cross_stderr, SV("expected an output path after '-o' option flag\n"));
				return result;
			}
			output_path = args[i_arg + 1];
		}
	}

	if (output_path.length == 0) {
			cross_log(cross_stderr, SV("An output path (-o) is required\n"));
			return 1;
	}

	VMConfig vm_config = {0};
	vm_config.load_module = on_load_module;
	vm_config.error_callback = on_error;
	VM *vm = vm_create(&memory_arena, vm_config);

	if (sv_equals(command, SV("deps")))
	{
		Arena temp_mem = memory_arena;
		char *file_content = arena_alloc(&temp_mem, 1024);
		StringBuilder sb = string_builder_from_buffer(file_content, 1024);
		bool success = cli_parse_module(temp_mem, vm, input_module, &sb);
		if (!success) {
			result = 1;
		} else if (sb.size > 0) {
			success = cross_write_entire_file(output_path, sb.buffer, sb.size);
			if (!success) {
				cross_log(cross_stderr, SV("failed to write dependencies to "));
				cross_log(cross_stderr, output_path);
				cross_log(cross_stderr, SV("\n"));
				result = 1;
			}
			
		}
	}
	else if (sv_equals(command, SV("build")))
	{
		byte content[] = {1, 2, 3, 4};
		bool success = cross_write_entire_file(output_path, content, sizeof(content));
		if (!success) {
			cross_log(cross_stderr, SV("failed to write binary module to "));
			cross_log(cross_stderr, output_path);
			cross_log(cross_stderr, SV("\n"));
			result = 1;
		}
	}
	else
	{
		cross_log(cross_stderr, SV("unknown command "));
		cross_log(cross_stderr, command);
		cross_log(cross_stderr, SV(", expected 'deps' or 'build'\n"));
	}

	vm_destroy(vm);
	
	result = 0;
	return result;
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
