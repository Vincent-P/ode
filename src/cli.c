// Compatibility crap
#include <stdint.h>
#define NULL 0
#define wchar_t uint16_t
// CRT stub
void *memset(void *dest, int c, size_t count);
void *memcpy(void *dest, const void *src, size_t count);
typedef _Bool bool;
enum Core_Constants
{
	false = 0,
	true = 1,
};

#define SOKOL_IMPL
#define SOKOL_ASSERT(c) do { if (!(c)) __debugbreak(); } while(0)
#include "sokol_time.h"

#if defined(UNITY_BUILD)
#include "debug.c"
#include "type_id.c"
#include "lexer.c"
#include "parser.c"
#include "compiler.c"
#include "vm.c"
#include "executor.c"
#include "cross.c"
#endif

#include "cross.h"
#include "arena.h"
#include "vm.h"

#define CLI_LOG 2

typedef char NameBuffer[64];
static struct {
	Arena persistent_arena;
	// options
	bool watch_opt;
	sv src_dir_arg;
	// modules
	VM *vm;
	sv main_module_arg;
	NameBuffer modules_name[32];
	sv modules_name_sv[32];
	NameBuffer modules_path[32];
	sv modules_path_sv[32];
	uint64_t modules_file_last_write[32];
	uint32_t modules_len;
} state;

typedef struct RenderRect
{
	float x;
	float y;
	float width;
	float height;
} RenderRect;

typedef struct RenderList
{
	Slice rects;
	uint32_t rects_length;
} RenderList;

struct OdeHeap
{
	RenderList render_list;
	RenderRect rects[128];
};
static struct OdeHeap ode_heap;

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
	// Don't count the NULL terminator in the length
	*out_path_length = i;
	// Terminate with 0
	out_path[i++] = 0;
	return true;
}

static void foreign_get_render_list(ExecutionContext *ctx, Value *stack, uint32_t arg_count, uint32_t *sp)
{
	if (arg_count != 0) {
		cross_log(cross_stderr, SV("===HOST: get_render_list: expected 0 arguments.\n"));
	}

	ode_heap.render_list.rects.ptr = make_heap_pointer(ctx, sizeof(RenderList)).ptr;

	*stack = make_heap_pointer(ctx, 0);
	*sp = *sp + 1;
}

static void dummy_foreign_func(ExecutionContext *ctx, Value *stack, uint32_t arg_count, uint32_t *sp)
{
	cross_log(cross_stdout, SV("Dummy foreign func.\n"));
}

static void log_foreign_func(ExecutionContext *ctx, Value *stack, uint32_t arg_count, uint32_t *sp)
{
	if (arg_count != 2) {
		cross_log(cross_stderr, SV("===HOST: log_foreign_func: expected 2 arguments\n"));
	}
	else {
		sv message = {0};
		message.chars = (const char*)deref_pointer(ctx, stack[0].ptr);
		message.length = stack[1].u32;

		char logbuf[128] = {0};
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		string_builder_append_sv(&sb, SV("=== HOST: log_foreign_func: \""));
		string_builder_append_sv(&sb, message);
		string_builder_append_sv(&sb, SV("\" ===\n"));
		cross_log(cross_stdout, string_builder_get_string(&sb));
	}
}

static void logi_foreign_func(ExecutionContext *ctx, Value *stack, uint32_t arg_count, uint32_t *sp)
{
	cross_log(cross_stderr, SV("logi foreign func.\n"));

	if (arg_count != 1) {
		cross_log(cross_stderr, SV("===HOST: logi_foreign_func: expected 1 argument ===\n"));
	}
	else {
		char logbuf[64] = {0};
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		string_builder_append_sv(&sb, SV("=== HOST: logi_foreign_func: "));
		string_builder_append_u64(&sb, (uint64_t)stack[0].u32);
		string_builder_append_sv(&sb, SV(" ===\n"));
		cross_log(cross_stdout, string_builder_get_string(&sb));
	}

}

static ForeignFn on_foreign(sv module_name, sv function_name)
{
	char logbuf[96] = {0};
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
	} else if (sv_equals(function_name, sv_from_null_terminated("get-render-list"))) {
		return foreign_get_render_list;
	}

	return dummy_foreign_func;
}

static void on_module_compiled(sv module_name)
{
#if CLI_LOG > 0
	char logbuf[64]	 = {0};
	StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
#endif
	// Search the module by name
	uint32_t i_module = 0;
	for (; i_module < state.modules_len; ++i_module) {
		if (sv_equals(state.modules_name_sv[i_module], module_name)) {
			break;
		}
	}
	if (i_module >= state.modules_len) {
#if CLI_LOG > 0
		string_builder_append_sv(&sb, SV("Cannot find compiled module: "));
		string_builder_append_sv(&sb, module_name);
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));
#endif
		return;
	}
	const uint64_t file_last_write = cross_get_file_last_write(
								   state.modules_path_sv[i_module].chars,
								   state.modules_path_sv[i_module].length);

	state.modules_file_last_write[i_module] = file_last_write;
}
static bool on_load_module(sv module_name, sv *out_code)
{
#if CLI_LOG > 0
	char logbuf[64]	 = {0};
	StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
#endif
	// Open the module file
	char module_path[64] = {0};
	uint32_t module_path_length = 64;
	bool success = get_module_path(module_path, &module_path_length, state.src_dir_arg, module_name);
	if (!success) {
		return false;
	}
	ReadFileResult file_content = cross_read_entire_file(module_path);
	if (!file_content.success) {
#if CLI_LOG > 0
		string_builder_append_sv(&sb, SV("Cannot open file "));
		string_builder_append_sv(&sb, (sv){module_path, module_path_length});
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));
#endif
		return false;
	}
#if CLI_LOG > 0
	string_builder_append_sv(&sb, SV("Load module: "));
	string_builder_append_sv(&sb, (sv){module_path, module_path_length});
	string_builder_append_char(&sb, '\n');
	cross_log(cross_stderr, string_builder_get_string(&sb));
#endif
	*out_code = file_content.content;
	// Register the module in our modules array
	uint32_t i_module = 0;
	for (; i_module < state.modules_len; ++i_module) {
		if (sv_equals(state.modules_name_sv[i_module], module_name)) {
			break;
		}
	}
	if (i_module >= state.modules_len) {
		// Add a new module
		i_module = state.modules_len;
		state.modules_len += 1;
		// Init name
		memcpy(&state.modules_name[i_module], module_name.chars, module_name.length);
		state.modules_name[i_module][module_name.length] = '\0';
		state.modules_name_sv[i_module].chars = state.modules_name[i_module];
		state.modules_name_sv[i_module].length = module_name.length;
		// Init path
		memcpy(&state.modules_path[i_module], module_path, sizeof(module_path));
		state.modules_path_sv[i_module].chars = state.modules_path[i_module];
		state.modules_path_sv[i_module].length = module_path_length;
		// Init time
		state.modules_file_last_write[i_module] = 0;
	}
	return true;
}

static void on_error(VM *vm, Error err)
{
	char buf[96] = {0};
	StringBuilder sb = string_builder_from_buffer(buf, sizeof(buf));
	if (err.file.chars != NULL) {
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


static void init(void)
{
	// Init sokol_timer
	stm_setup();

	// Init ode
	uint32_t persistent_memory_size = 1 << 20;
	uint8_t *persistent_memory = (uint8_t*)cross_alloc(persistent_memory_size);
	state.persistent_arena.begin = persistent_memory;
	state.persistent_arena.end = persistent_memory + persistent_memory_size;

	// Setup the main module
	// Add the module as module#0
	state.modules_len = 1;
	// Init name
	state.modules_name_sv[0] = state.main_module_arg;
	// Init path
	state.modules_path_sv[0].chars = state.modules_path[0];
	state.modules_path_sv[0].length = 64; // buffer size is 64, get_module_path will replace it with real length
	if (!get_module_path(state.modules_path[0], &state.modules_path_sv[0].length, state.src_dir_arg, state.main_module_arg)) {
		return;
	}
	// Init time
	state.modules_file_last_write[0] = 0;

	// Create the VM
	VMConfig vm_config = {0};
	vm_config.load_module = on_load_module;
	vm_config.on_module_compiled = on_module_compiled;
	vm_config.error_callback = on_error;
	vm_config.foreign_callback = on_foreign;
	vm_config.heap = (uint8_t*)&ode_heap;
	state.vm = vm_create(&state.persistent_arena, vm_config);
}

static void frame(void)
{
#if CLI_LOG > 0
 	char buf[128] = {0};
	StringBuilder sb = string_builder_from_buffer(buf, sizeof(buf));
#endif

	// Update ode VM
	bool any_module_changed = true;
	for (uint32_t i_module = 0; i_module < state.modules_len; ++i_module) {
		const uint64_t last_write = cross_get_file_last_write(state.modules_path_sv[i_module].chars, state.modules_path_sv[i_module].length);
		if (last_write > state.modules_file_last_write[i_module]) {
			state.modules_file_last_write[i_module] = last_write;
			any_module_changed = true;

			// read entire source code
			ReadFileResult file_content = cross_read_entire_file(state.modules_path[i_module]);
			if (!file_content.success) {
#if CLI_LOG > 0
				string_builder_append_sv(&sb, SV("Cannot open file "));
				string_builder_append_sv(&sb, state.modules_path_sv[i_module]);
				string_builder_append_char(&sb, '\n');
				cross_log(cross_stderr, string_builder_get_string(&sb));
#endif
				return;
			}
			// compile
#if CLI_LOG > 0
			string_builder_append_sv(&sb, SV("Compile module: "));
			string_builder_append_sv(&sb, state.modules_path_sv[i_module]);
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
#endif

			Error error = vm_compile(state.vm, state.modules_name_sv[i_module], file_content.content);
			if (error.code != ErrorCode_Ok) {
				on_error(state.vm, error);
			}

		} else {
#if CLI_LOG > 1
			string_builder_append_sv(&sb, SV("Skipping module: "));
			string_builder_append_sv(&sb, state.modules_path_sv[i_module]);
			string_builder_append_sv(&sb, SV(" because it is cached.\n"));
			cross_log(cross_stderr, string_builder_get_string(&sb));
#endif		  	 
		}
	}
	if (any_module_changed) {
		vm_call(state.vm, state.modules_name_sv[0], sv_from_null_terminated("main"), state.persistent_arena);
#if CLI_LOG > 0
		uint64_t timestamp = stm_now();

		char buf[32] = {0};
		StringBuilder sb = string_builder_from_buffer(buf, sizeof(buf));
		string_builder_append_sv(&sb, SV("Done executing at "));
		string_builder_append_u64(&sb, timestamp);
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stdout, string_builder_get_string(&sb));
#endif
	}
}

static void cleanup(void) {}

// CRT

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

int WinMainCRTStartup(void);
int WinMainCRTStartup(void)
{
	cross_init();
	state.src_dir_arg = SV("src");
	state.main_module_arg = SV("test");
	init();
	frame();
	cleanup();
	ExitProcess(0);
	return 0;
}
