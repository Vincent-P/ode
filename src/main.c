#if defined(UNITY_BUILD)
#include "debug.c"
#include "lexer.c"
#include "parser.c"
#include "compiler.c"
#include "vm.c"
#include "executor.c"
#include "cross.c"
#endif

#define _CRT_SECURE_NO_WARNINGS
#define SOKOL_IMPL
#define SOKOL_ASSERT(x)
#include "sokol_time.h"

#include "cross.h"
#include "arena.h"
#include "vm.h"

// options
sv src_dir_arg = {0};
// modules
typedef char NameBuffer[64];
NameBuffer modules_name[32];
sv modules_name_sv[32];
NameBuffer modules_path[32];
sv modules_path_sv[32];
uint64_t modules_file_last_write[32];
uint32_t modules_len;

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

void dummy_foreign_func(void)
{
        cross_log(cross_stdout, SV("Dummy foreign func.\n"));
}

void log_foreign_func(void)
{
        // StackValue arg0 = execution_get_local(ctx, 0);
        // sv arg0_sv = execution_get_str(ctx, arg0.str);
        // printf("\n=== HOST: log(\"%.*s\") ===\n", int(arg0_sv.length), arg0_sv.chars);
        cross_log(cross_stderr, SV("log foreign func.\n"));
}

void logi_foreign_func(void)
{
        // StackValue n = execution_get_local(ctx, 0);
        // printf("\n=== HOST: log(%d) ===\n", n.i32);
        cross_log(cross_stderr, SV("logi foreign func.\n"));
}

ForeignFn on_foreign(sv module_name, sv function_name)
{
	char logbuf[64] = {0};
	StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
	string_builder_append_sv(&sb, SV("=== HOST: On foreign function: module "));
	string_builder_append_sv(&sb, module_name);
	string_builder_append_sv(&sb, SV(" function "));
	string_builder_append_sv(&sb, function_name);
	string_builder_append_sv(&sb, SV(" ===\n"));
	cross_log(cross_stdout, string_builder_get_string(&sb));

        if (sv_equals(function_name, sv_from_null_terminated("log"))) {
                return log_foreign_func;
        } else if (sv_equals(function_name, sv_from_null_terminated("logi"))) {
                return logi_foreign_func;
        }

        return dummy_foreign_func;
}

bool on_load_module(sv module_name, sv *out_code)
{
	char logbuf[64]  = {0};
	StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
	
        // Open the module file
        char module_path[64] = {0};
        uint32_t module_path_length = 64;
        bool success = get_module_path(module_path, &module_path_length, src_dir_arg, module_name);
        if (!success) {
                return false;
        }
        ReadFileResult file_content = cross_read_entire_file(module_path);
	if (!file_content.success) {
		string_builder_append_sv(&sb, SV("Cannot open file "));
		string_builder_append_sv(&sb, (sv){module_path, module_path_length});
		string_builder_append_char(&sb, '\n');
                cross_log(cross_stderr, string_builder_get_string(&sb));
                return false;
	}
	string_builder_append_sv(&sb, SV("Load module: "));
	string_builder_append_sv(&sb, (sv){module_path, module_path_length-1});
	string_builder_append_char(&sb, '\n');
        cross_log(cross_stderr, string_builder_get_string(&sb));

        *out_code = file_content.content;
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

void on_error(VM *vm, Error err)
{
	char buf[96] = {0};
	StringBuilder sb = string_builder_from_buffer(buf, sizeof(buf));
        if (err.file.chars != nullptr) {
		// <file>:<line>:0: error: 
		string_builder_append_sv(&sb, err.file);
		string_builder_append_char(&sb, ':');
		string_builder_append_u64(&sb, (uint64_t)(err.line));
		string_builder_append_sv(&sb, SV(":0: error: "));
        }
	string_builder_append_sv(&sb, SV(ErrorCode_str[(uint32_t)(err.code)]));
	// EXECUTOR FAILED at (<ip>): <msg>
	string_builder_append_sv(&sb, SV(" EXECUTOR FAILED at ("));
	string_builder_append_u64(&sb, (uint64_t)(err.ip));
	string_builder_append_sv(&sb, SV("): "));
	string_builder_append_sv(&sb, err.msg);
	string_builder_append_char(&sb, '\n');
	cross_log(cross_stderr, string_builder_get_string(&sb));
}

int main(int argc, const char *argv[])
{
	argc = 3;
	argv[0] = "";
	argv[1] = "src";
	argv[2] = "test";
        if (argc < 3) {
                cross_log(cross_stderr, sv_from_null_terminated("Usage: ode.exe <src dir> <main_module> (-w)\n"));
                return 1;
        }

	uint32_t persistent_memory_size = 1 << 20;
	uint8_t *persistent_memory = (uint8_t*)cross_alloc(persistent_memory_size);
	Arena persistent_arena = {0};
	persistent_arena.begin = persistent_memory;
	persistent_arena.end = persistent_memory + persistent_memory_size;
	
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
        VMConfig vm_config = {0};
        vm_config.load_module = on_load_module;
        vm_config.error_callback = on_error;
        vm_config.foreign_callback = on_foreign;
        VM *vm = vm_create(&persistent_arena, vm_config);
        // Watch and execute
        bool stop = !watch_opt;
        do {
                bool any_module_changed = false;
                for (uint32_t i_module = 0; i_module < modules_len; ++i_module) {
                        const uint64_t last_write = cross_get_file_last_write(modules_path_sv[i_module].chars, modules_path_sv[i_module].length);
                        if (last_write != modules_file_last_write[i_module]) {
                                modules_file_last_write[i_module] = last_write;
                                any_module_changed = true;
    
                                // read entire source code
                                ReadFileResult file_content = cross_read_entire_file(modules_path[i_module]);
				if (!file_content.success) {
					char buf[64] = {0};
					StringBuilder sb = string_builder_from_buffer(buf, sizeof(buf));
					string_builder_append_sv(&sb, SV("Cannot open file "));
					string_builder_append_sv(&sb, modules_path_sv[i_module]);
					string_builder_append_char(&sb, '\n');			
					cross_log(cross_stderr, string_builder_get_string(&sb));
                                        return 1;
				}
                                // compile
                                Error error = vm_compile(vm, modules_name_sv[i_module], file_content.content);
				if (error.code != ErrorCode_Ok) {
					on_error(vm, error);
				}
                        }
                }

                if (any_module_changed) {
                        vm_call(vm, modules_name_sv[0], sv_from_null_terminated("main"), persistent_arena);
			uint64_t timestamp = stm_now();

			char buf[32] = {0};
			StringBuilder sb = string_builder_from_buffer(buf, sizeof(buf));
			string_builder_append_sv(&sb, SV("Done executing at "));
			string_builder_append_u64(&sb, timestamp);
			string_builder_append_char(&sb, '\n');			
			cross_log(cross_stdout, string_builder_get_string(&sb));
                }
  
                cross_sleep_ms(33);
        } while (!stop);
        return 0;
}

void *memset(void *dest, int c, size_t count)
{
	char *bytes = (char *)dest;
	while (count--)
	{
		*bytes++ = (char)c;
	}
	return dest;
}

void *memcpy(void *dest, const void *src, size_t count)
{
	char *dest8 = (char *)dest;
	const char *src8 = (const char *)src;
	while (count--)
	{
		*dest8++ = *src8++;
	}
	return dest;
}

void *memmove(void *dest, const void *src, size_t count)
{
	char *dest8 = (char *)dest;
	const char *src8 = (const char *)src;
	while (count--)
	{
		*dest8++ = *src8++;
	}
	return dest;
}

int _fltused;
	
int mainCRTStartup(void)
{
	cross_init();

	int argc = 0;
	const char* argv[5] = {0};
	main(argc, argv);
	return 0;
}
