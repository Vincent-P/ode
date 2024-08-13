// Compatibility crap
#include <stdint.h>
#if !defined(NULL)
#define NULL 0
#endif
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


// SOKOL crap
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wunused-but-set-variable"
#define SOKOL_IMPL
#define SOKOL_ASSERT(c) do { if (!(c)) __debugbreak(); } while(0)
#define SOKOL_D3D11
#define SOKOL_NO_ENTRY
#include "sokol_time.h"
#include "sokol_app.h"
#include "sokol_gfx.h"
#include "sokol_glue.h"
#pragma clang diagnostic pop

// 
#if defined(UNITY_BUILD)
#include "debug.c"
#include "type_id.c"
#include "lexer.c"
#include "parser.c"
#include "compiler.c"
#include "vm.c"
#include "executor.c"
#include "core/cross.c"
#endif

#include "core/core.h"
#include "core/cross.h"
#include "vm.h"

static void *win32_malloc(size_t s, void* user_data) { return HeapAlloc(GetProcessHeap(), 0, s); }
static void win32_free(void* p, void* user_data) { HeapFree(GetProcessHeap(), 0, p); }

typedef char NameBuffer[64];
static struct {
	Arena persistent_arena;
	// rendering
	sg_pipeline pip;
	sg_bindings bind;
	sg_pass_action pass_action;
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

struct Input
{
	int up;
	int down;
};

struct OdeHeap
{
	RenderList render_list;
	RenderRect rects[128];
	union
	{
		struct Input input;
		uint8_t input_bytes[1024];
	};
	uint8_t game_state[1024];
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
	// Terminate with 0
	out_path[i++] = 0;
	*out_path_length = i;
	return true;
}

static void foreign_get_render_list(ExecutionContext *ctx, Value *stack, uint32_t arg_count, uint32_t *sp)
{
	if (arg_count != 0) {
		cross_log(cross_stderr, SV("===HOST: get_render_list: expected 0 arguments.\n"));
	}

	ode_heap.render_list.rects.ptr = make_heap_pointer(ctx, offsetof(struct OdeHeap, rects)).ptr;
	
	*stack = make_heap_pointer(ctx, 0);
	*sp = *sp + 1;
}

static void foreign_get_game_state(ExecutionContext *ctx, Value *stack, uint32_t arg_count, uint32_t *sp)
{
	if (arg_count != 0) {
		cross_log(cross_stderr, SV("===HOST: get-game-state: expected 0 arguments.\n"));
	}

	*stack = make_heap_pointer(ctx, offsetof(struct OdeHeap, game_state));
	*sp = *sp + 1;
}

static void foreign_get_input(ExecutionContext *ctx, Value *stack, uint32_t arg_count, uint32_t *sp)
{
	if (arg_count != 0) {
		cross_log(cross_stderr, SV("===HOST: get-input: expected 0 arguments.\n"));
	}

	*stack = make_heap_pointer(ctx, offsetof(struct OdeHeap, input));
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
static void logf_foreign_func(ExecutionContext *ctx, Value *stack, uint32_t arg_count, uint32_t *sp)
{
	cross_log(cross_stderr, SV("logf foreign func.\n"));

	if (arg_count != 1) {
		cross_log(cross_stderr, SV("===HOST: logf_foreign_func: expected 1 argument ===\n"));
	}
	else {
		char logbuf[64] = {0};
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		string_builder_append_sv(&sb, SV("=== HOST: logf_foreign_func: "));
		string_builder_append_f32(&sb, stack[0].f32);
		string_builder_append_sv(&sb, SV(" ===\n"));
		cross_log(cross_stdout, string_builder_get_string(&sb));
	}
}

static ForeignFn on_foreign(sv module_name, sv function_name)
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
	} else if (sv_equals(function_name, sv_from_null_terminated("logb"))) {
		return logi_foreign_func;
	} else if (sv_equals(function_name, sv_from_null_terminated("logf"))) {
		return logf_foreign_func;
	} else if (sv_equals(function_name, sv_from_null_terminated("get-render-list"))) {
		return foreign_get_render_list;
	} else if (sv_equals(function_name, sv_from_null_terminated("get-game-state"))) {
		return foreign_get_game_state;
	} else if (sv_equals(function_name, sv_from_null_terminated("get-input"))) {
		return foreign_get_input;
	}

	return dummy_foreign_func;
}

static bool on_load_module(sv module_name, sv *out_code)
{
	char logbuf[64]	 = {0};
	StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
	
	// Open the module file
	char module_path[64] = {0};
	uint32_t module_path_length = 64;
	bool success = get_module_path(module_path, &module_path_length, state.src_dir_arg, module_name);
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

static void sokol_logger (
			  const char* tag,		  // always "sapp"
			  uint32_t log_level,		  // 0=panic, 1=error, 2=warning, 3=info
			  uint32_t log_item_id,		  // SAPP_LOGITEM_*
			  const char* message_or_null,	  // a message string, may be nullptr in release mode
			  uint32_t line_nr,		  // line number in sokol_app.h
			  const char* filename_or_null,	  // source filename, may be nullptr in release mode
			  void* user_data)
{
	char buf[256] = {0};
	StringBuilder sb = string_builder_from_buffer(buf, sizeof(buf));

	if (filename_or_null) {
		string_builder_append_sv(&sb, sv_from_null_terminated(filename_or_null));
	}
	string_builder_append_char(&sb, ':');
	string_builder_append_u64(&sb, (uint64_t)line_nr);
	string_builder_append_char(&sb, ':');
	string_builder_append_char(&sb, ' ');
	if (log_level == 0) {
		string_builder_append_sv(&sb, SV("panic"));
	} else if (log_level == 1) {
		string_builder_append_sv(&sb, SV("error"));
	} else if (log_level == 2) {
		string_builder_append_sv(&sb, SV("warning"));
	} else if (log_level == 3) {
		string_builder_append_sv(&sb, SV("info"));
	}
	string_builder_append_sv(&sb, SV(": "));
	if (message_or_null) {
		string_builder_append_sv(&sb, sv_from_null_terminated(message_or_null));
	}
	string_builder_append_char(&sb, '\n');
	cross_log(cross_stderr, string_builder_get_string(&sb));

	if (log_level <= 1) {
		// __debugbreak();
	}
}

static void init(void)
{
	// Init sokol_timer
	stm_setup();
	// Init sokol_gfx
	sg_setup(&(sg_desc){
		.context = sapp_sgcontext(),
		.logger.func = sokol_logger,
		.allocator.alloc_fn = win32_malloc,
		.allocator.free_fn = win32_free,
	});
	state.bind.vertex_buffers[0] = sg_make_buffer(&(sg_buffer_desc){
		.usage = SG_USAGE_STREAM,
		.label = "triangle-vertices",
		.size = 128 * 6 * 7*sizeof(float),
	});
	sg_shader triangle_shader = sg_make_shader(&(sg_shader_desc){
		.attrs = {
			[0].sem_name = "POS",
			[1].sem_name = "COLOR"
		},
		.vs.source =
	    "struct vs_in {\n"
	    "  float4 pos: POS;\n"
	    "  float4 color: COLOR;\n"
	    "};\n"
	    "struct vs_out {\n"
	    "  float4 color: COLOR0;\n"
	    "  float4 pos: SV_Position;\n"
	    "};\n"
	    "vs_out main(vs_in inp) {\n"
	    "  vs_out outp;\n"
	    "  outp.pos = inp.pos;\n"
	    "  outp.color = inp.color;\n"
	    "  return outp;\n"
	    "}\n",
		.fs.source =
	    "float4 main(float4 color: COLOR0): SV_Target0 {\n"
	    "  return color;\n"
	    "}\n"
	});	
	state.pip = sg_make_pipeline(&(sg_pipeline_desc){
		.shader = triangle_shader,
		.layout = {
			.attrs = {
				[0].format = SG_VERTEXFORMAT_FLOAT3,
				[1].format = SG_VERTEXFORMAT_FLOAT4
			}
		},
		.label = "triangle-pipeline"
	});
	state.pass_action = (sg_pass_action) {
		.colors[0] = { .load_action=SG_LOADACTION_CLEAR, .clear_value={0.0f, 0.0f, 0.0f, 1.0f } }
	};

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
	vm_config.error_callback = on_error;
	vm_config.foreign_callback = on_foreign;
	vm_config.heap = (uint8_t*)&ode_heap;
	state.vm = vm_create(&state.persistent_arena, vm_config);
}

static void event(const sapp_event* event)
{
	if (event->type == SAPP_EVENTTYPE_KEY_DOWN) {
		if (event->key_code == SAPP_KEYCODE_UP) {
			ode_heap.input.up = 1;
		} else if (event->key_code == SAPP_KEYCODE_DOWN) {
			ode_heap.input.down = 1;
		} else if (event->key_code == SAPP_KEYCODE_R) {
			ode_heap = (struct OdeHeap){0};
		}
	} else if (event->type == SAPP_EVENTTYPE_KEY_UP) {
		if (event->key_code == SAPP_KEYCODE_UP) {
			ode_heap.input.up = 0;
		} else if (event->key_code == SAPP_KEYCODE_DOWN) {
			ode_heap.input.down = 0;
		}
	}
}

static void frame(void)
{
	// Update ode VM
	bool any_module_changed = false;
	for (uint32_t i_module = 0; i_module < state.modules_len; ++i_module) {
		const uint64_t last_write = cross_get_file_last_write(state.modules_path_sv[i_module].chars, state.modules_path_sv[i_module].length);
		if (last_write != state.modules_file_last_write[i_module]) {
			state.modules_file_last_write[i_module] = last_write;
			any_module_changed = true;
    
			// read entire source code
			ReadFileResult file_content = cross_read_entire_file(state.modules_path[i_module]);
			if (!file_content.success) {
				char buf[64] = {0};
				StringBuilder sb = string_builder_from_buffer(buf, sizeof(buf));
				string_builder_append_sv(&sb, SV("Cannot open file "));
				string_builder_append_sv(&sb, state.modules_path_sv[i_module]);
				string_builder_append_char(&sb, '\n');			
				cross_log(cross_stderr, string_builder_get_string(&sb));
				return;
			}
			// compile
			Error error = vm_compile(state.persistent_arena, state.vm, state.modules_name_sv[i_module], file_content.content);
			if (error.code != ErrorCode_Ok) {
				on_error(state.vm, error);
			}
		}
	}
	if (any_module_changed) {
		vm_call(state.vm, state.modules_name_sv[0], sv_from_null_terminated("main"), state.persistent_arena);
		uint64_t timestamp = stm_now();

		char buf[32] = {0};
		StringBuilder sb = string_builder_from_buffer(buf, sizeof(buf));
		string_builder_append_sv(&sb, SV("Done executing at "));
		string_builder_append_u64(&sb, timestamp);
		string_builder_append_char(&sb, '\n');			
		cross_log(cross_stdout, string_builder_get_string(&sb));
	}

	memset(ode_heap.rects, 0,  sizeof(ode_heap.rects));
	ode_heap.render_list = (struct RenderList){0};
	ode_heap.render_list.rects.length = ARRAY_LENGTH(ode_heap.rects);
	
	vm_call(state.vm, state.modules_name_sv[0], sv_from_null_terminated("update"), state.persistent_arena);

	float vertices[128*6*7] = {0};
	uint32_t i_vert_attr = 0;
	int32_t vert_count = 0;
	for (uint32_t i_rect = 0; i_rect < ode_heap.render_list.rects_length; ++i_rect)
	{
		RenderRect r = ode_heap.rects[i_rect];
		// 0 1
		// 2 3

		// 0
		vertices[i_vert_attr++] = r.x; vertices[i_vert_attr++] = r.y; vertices[i_vert_attr++] = 0.0f;
		vertices[i_vert_attr++] = 1.0f; vertices[i_vert_attr++] = 0.0f; vertices[i_vert_attr++] = 1.0f; vertices[i_vert_attr++] = 1.0f;
		// 2
		vertices[i_vert_attr++] = r.x; vertices[i_vert_attr++] = r.y + r.height; vertices[i_vert_attr++] = 0.0f;
		vertices[i_vert_attr++] = 1.0f; vertices[i_vert_attr++] = 0.0f; vertices[i_vert_attr++] = 1.0f; vertices[i_vert_attr++] = 1.0f;	
		// 1
		vertices[i_vert_attr++] = r.x + r.width; vertices[i_vert_attr++] = r.y; vertices[i_vert_attr++] = 0.0f;
		vertices[i_vert_attr++] = 1.0f; vertices[i_vert_attr++] = 0.0f; vertices[i_vert_attr++] = 1.0f; vertices[i_vert_attr++] = 1.0f;

		// 2
		vertices[i_vert_attr++] = r.x; vertices[i_vert_attr++] = r.y + r.height; vertices[i_vert_attr++] = 0.0f;
		vertices[i_vert_attr++] = 1.0f; vertices[i_vert_attr++] = 0.0f; vertices[i_vert_attr++] = 0.0f; vertices[i_vert_attr++] = 1.0f;
		// 3
		vertices[i_vert_attr++] = r.x + r.width; vertices[i_vert_attr++] = r.y + r.height; vertices[i_vert_attr++] = 0.0f;
		vertices[i_vert_attr++] = 1.0f; vertices[i_vert_attr++] = 0.0f; vertices[i_vert_attr++] = 0.0f; vertices[i_vert_attr++] = 1.0f;	
		// 1
		vertices[i_vert_attr++] = r.x + r.width; vertices[i_vert_attr++] = r.y; vertices[i_vert_attr++] = 0.0f;
		vertices[i_vert_attr++] = 1.0f; vertices[i_vert_attr++] = 0.0f; vertices[i_vert_attr++] = 0.0f; vertices[i_vert_attr++] = 1.0f;

		vert_count += 6;
	}
	sg_range new_vertices = SG_RANGE(vertices);
	new_vertices.size = i_vert_attr * sizeof(float);
	if (new_vertices.size > 0) {
		sg_update_buffer(state.bind.vertex_buffers[0], &new_vertices);
	}

	
	// Render frame
	sg_begin_default_pass(&state.pass_action, sapp_width(), sapp_height());
	sg_apply_pipeline(state.pip);
	sg_apply_bindings(&state.bind);
	sg_draw(0, vert_count, 1);
	sg_end_pass();
	sg_commit();

	// sapp_quit();
}

static void cleanup(void)
{
	sg_shutdown();
}

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
int __DllMainCRTStartup(HANDLE hinstDLL, DWORD dwReason, LPVOID lpvReserved);

int WinMainCRTStartup(void)
{
	cross_init();

	// Paths
	state.src_dir_arg = SV("src");
	state.main_module_arg = SV("test");
	sapp_desc desc =  (sapp_desc){
		.init_cb = init,
		.frame_cb = frame,
		.cleanup_cb = cleanup,
		.event_cb = event,
		.width = 640,
		.height = 480,
		.window_title = "Ode demo",
		.icon.sokol_default = true,
		.logger.func = sokol_logger,
		.allocator.alloc_fn = win32_malloc,
		.allocator.free_fn = win32_free,
	};
	sapp_run(&desc);
	ExitProcess(0);
	return 0;
}
