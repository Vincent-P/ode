#include "vm.h"
#include "arena.h"
#include "compiler.h"
#include "debug.h"
#include "executor.h"
#include "lexer.h"
#include "opcodes.h"
#include "parser.h"

VM *vm_create(Arena *arena, VMConfig config)
{
	VM *vm = (VM*)arena_alloc(arena, sizeof(VM));
	vm->config = config;
	return vm;
}

void vm_destroy(VM *vm)
{
	(void)vm;
}

typedef struct CompilationResult
{
	Error error;
	uint32_t i_compiler_module;
} CompilationResult;

typedef struct LoadModuleResult
{
	Error error;
	uint32_t i_runtime_module;
} LoadModuleResult;

typedef struct CompileLoadLinkResult
{
	Error error;
	uint32_t i_compiler_module;
	uint32_t i_runtime_module;
} CompileLoadLinkResult;

CompileLoadLinkResult compile_load_link_code(VM *vm, sv module_name, sv code);

// Parse code, compile module.
static CompilationResult compile_code(VM *vm, sv module_name, sv code)
{
	char logbuf[128] = {0};
	
	CompilationResult result = {0};

	CompilationUnit compunit = {0};
	compunit.input = code;
	
	// -- Lex tokens
	lexer_scan(&compunit);
	if (compunit.error.code != ErrorCode_Ok) {
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		// <file>:<line>:0: error Lexer[] returned <errcode>
		string_builder_append_sv(&sb, compunit.error.file);
		string_builder_append_char(&sb, ':');
		string_builder_append_u64(&sb, (uint64_t)(compunit.error.line));
		string_builder_append_sv(&sb, SV(":0: error: Lexer[] returned "));
		string_builder_append_sv(&sb, SV(ErrorCode_str[(uint32_t)(compunit.error.code)]));
		string_builder_append_char(&sb, '\n');
		// Error at: '<errorstr>'
		build_error_at(code, compunit.error.span, &sb);
		cross_log(cross_stderr, string_builder_get_string(&sb));
		
		result.error = compunit.error;
		return result;
	}

	
	// -- Parse tokens into a parse tree
	Parser parser = {0};
	parser.compunit = &compunit;
	parse_module(&parser);
	if (compunit.error.code != ErrorCode_Ok) {
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));

		// <file>:<line>:0: error Parser[] returned <errcode>
		string_builder_append_sv(&sb, compunit.error.file);
		string_builder_append_char(&sb, ':');
		string_builder_append_u64(&sb, (uint64_t)(compunit.error.line));
		string_builder_append_sv(&sb, SV(":0: error: Parser[token_length: "));
		string_builder_append_u64(&sb, (uint64_t)(compunit.tokens_length));
		string_builder_append_sv(&sb, SV(", i_current_token: "));
		string_builder_append_u64(&sb, (uint64_t)(parser.i_current_token));
		string_builder_append_sv(&sb, SV("] returned "));
		string_builder_append_sv(&sb, SV(ErrorCode_str[(uint32_t)(compunit.error.code)]));
		string_builder_append_sv(&sb, SV("'\n"));
		cross_log(cross_stderr, string_builder_get_string(&sb));
		build_error_at(code, compunit.error.span, &sb);
		cross_log(cross_stderr, string_builder_get_string(&sb));

		if (compunit.tokens_length > 0) {
			uint32_t i_last_token =
				parser.i_current_token < compunit.tokens_length ? parser.i_current_token : compunit.tokens_length - 1;
			const Token *last_token = compunit.tokens + i_last_token;
			sv last_token_str = sv_substr(compunit.input, last_token->span);
			const char *token_kind_str = TokenKind_str[(uint32_t)(last_token->kind)];

			string_builder_append_sv(&sb, SV("# Last seen token is "));
			string_builder_append_sv(&sb, SV(token_kind_str));
			string_builder_append_char(&sb, '[');
			string_builder_append_sv(&sb, last_token_str);
			string_builder_append_char(&sb, ']');
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
		}
		if (parser.expected_token_kind != TokenKind_Invalid) {
			string_builder_append_sv(&sb, SV("# Expected token of kind"));
			string_builder_append_sv(&sb, SV(TokenKind_str[(uint32_t)(parser.expected_token_kind)]));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));

		}
		result.error = compunit.error;
		return result;
	}

	// -- Compile dependencies first
	const Token *require_paths[16] = {0};
	uint32_t dep_module_indices[16] = {0};
	uint32_t require_paths_length = 0;
	compiler_scan_requires(&compunit, require_paths, 16, &require_paths_length);
	if (require_paths_length == 16) {
		INIT_ERROR(&compunit.error, ErrorCode_Fatal);
		cross_log(cross_stderr, SV("Too much requires!\n"));
	}
	if (compunit.error.code != ErrorCode_Ok) {
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		Error err = compunit.error;
		string_builder_append_sv(&sb, err.file);
		string_builder_append_char(&sb, ':');
		string_builder_append_u64(&sb, (uint64_t)(err.line));
		string_builder_append_sv(&sb, SV(":0: error: scan_requires[] returned "));
		string_builder_append_sv(&sb, SV(ErrorCode_str[(uint32_t)(err.code)]));
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));
		result.error = compunit.error;
		return result;
	}
	for (uint32_t i_dep = 0; i_dep < require_paths_length; ++i_dep) {
		sv dep_module_name = sv_substr(code, require_paths[i_dep]->span);
		// HACK: Because string literals are substring of the code (and not properly interned), we have double quotes
		dep_module_name.chars += 1;
		dep_module_name.length -= 2;
		sv dep_input = {0};
		// Get the code from the load_module callback
		bool loading_success = vm->config.load_module(dep_module_name, &dep_input);
		if (!loading_success) {
			INIT_ERROR(&compunit.error, ErrorCode_Fatal);
			result.error = compunit.error;
			return result;
		}

		CompileLoadLinkResult dep_result = compile_load_link_code(vm, dep_module_name, dep_input);
		if (dep_result.error.code != ErrorCode_Ok) {
			result.error = dep_result.error;
			return result;
		}
		dep_module_indices[i_dep] = dep_result.i_compiler_module;
	}
	
	// -- Compile the parse tree into bytecode
	Compiler compiler = {0};
	compiler.vm = vm;
	compiler.compunit = &compunit;
	compiler.module.name = module_name;
	compiler.module.imports = dep_module_indices;
	compiler.module.imports_length = require_paths_length;
	compile_module(&compiler);
	if (compunit.error.code != ErrorCode_Ok) {
		Error err = compunit.error;
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		// <file>:<line>:0: error Compiler[] returned <errcode>
		string_builder_append_sv(&sb, err.file);
		string_builder_append_char(&sb, ':');
		string_builder_append_u64(&sb, (uint64_t)(err.line));
		string_builder_append_sv(&sb, SV(":0: error: Compiler[] returned "));
		string_builder_append_sv(&sb, SV(ErrorCode_str[(uint32_t)(err.code)]));
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));

		// Error at: <error_str>
		build_error_at(code, compunit.error.span, &sb);
		cross_log(cross_stderr, string_builder_get_string(&sb));

		// # expected type <type_str>
		string_builder_append_sv(&sb, SV("# expected type "));
		type_build_string(&sb, err.expected_type);
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));

		// # got type <type_str>
		string_builder_append_sv(&sb, SV("# got type "));
		type_build_string(&sb, err.got_type);
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));
		
		result.error = compunit.error;
		return result;
	}
	cross_log(cross_stderr, SV("\nCompilation success:\n"));
	for (uint32_t i_import = 0; i_import < compiler.module.imported_functions_length; ++i_import)
	{
		uint32_t i_imported_module = compiler.module.imported_module_indices[i_import];
		uint32_t i_imported_function = compiler.module.imported_function_indices[i_import];
		CompilerModule *imported_module = compiler.vm->compiler_modules + i_imported_module;
		sv imported_module_name = imported_module->name;
		sv imported_function_name = imported_module->functions[i_imported_function].name;

		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		string_builder_append_sv(&sb, SV("imported function: "));
		string_builder_append_sv(&sb, imported_module_name);
		string_builder_append_char(&sb, ' ');
		string_builder_append_sv(&sb, imported_function_name);
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));
	}
	print_bytecode(compiler.module.bytecode, compiler.module.bytecode_length);

	// -- Copy the new module to the VM
	uint32_t i_module = 0;
	for (; i_module < vm->compiler_modules_length; ++i_module) {
		if (sv_equals(vm->compiler_modules[i_module].name, module_name)) {
			break;
		}
	}
	if (i_module >= vm->compiler_modules_length) {
		if (i_module >= ARRAY_LENGTH(vm->compiler_modules)) {
			cross_log(cross_stderr, SV("Fatal: module capacity\n"));
			INIT_ERROR(&compunit.error, ErrorCode_Fatal);
			result.error = compunit.error;
			return result;
		}
		i_module = vm->compiler_modules_length;
		vm->compiler_modules_length += 1;
	}
	
	vm->compiler_modules[i_module] = compiler.module;
	
	result.i_compiler_module =  i_module;
	return result;
}

// This function could be used to load runtime modules from disk!
static void link_runtime_module(VM *vm, uint32_t i_runtime_module)
{
	// Link with other runtime modules, checking if they import a function from the new runtime module.
	Module *runtime_module = vm->runtime_modules + i_runtime_module;
	const uint32_t our_import_length = runtime_module->import_length;
	const uint32_t our_export_length = runtime_module->export_length;

	uint32_t runtime_modules_length = vm->runtime_modules_length;
	for (uint32_t m = 0; m < runtime_modules_length; ++m) {
		if (m != i_runtime_module) {
			Module *other_module = vm->runtime_modules + m;
			const uint32_t other_module_export_length = other_module->export_length;
			const uint32_t other_module_import_length = other_module->import_length;

			// Try to link our imports with other modules export by name
			for (uint32_t i_export = 0; i_export < other_module_export_length; ++i_export) {
				for (uint32_t i_import = 0; i_import < our_import_length; ++i_import) {
					if (sv_equals(runtime_module->import_names[i_import], other_module->export_names[i_export])) {
						if (runtime_module->import_addresses[i_import] != 0) {
							// Import already resolved?
							// Multiple symbols!
							__debugbreak();
						}
						runtime_module->import_module[i_import] = m;
						runtime_module->import_addresses[i_import] = other_module->export_addresses[i_export];
					}
				}
			}

			// We also want to try to fix our export on other modules, if they depend on us
			for (uint32_t i_import = 0; i_import < other_module_import_length; ++i_import) {
				if (sv_equals(other_module->import_module_names[i_import], runtime_module->name)) {
					// Reset other module import to 0
					other_module->import_addresses[i_import] = 0;
				
					for (uint32_t i_export = 0; i_export < our_export_length; ++i_export) {	
						if (sv_equals(other_module->import_names[i_import], runtime_module->export_names[i_export])) {
							other_module->import_module[i_import] = i_runtime_module;
							other_module->import_addresses[i_import] = runtime_module->export_addresses[i_export];
							break;
						}
					}
				}
			}
		}
	}

	for (uint32_t i_import = 0; i_import < our_import_length; ++i_import) {
		if (runtime_module->import_addresses[i_import] == 0) {
			// Unresolved import
			__debugbreak();
		}
	}

}

static LoadModuleResult load_compiler_module(VM *vm, uint32_t i_compiler_module)
{
	LoadModuleResult result = {0};

	// Find or create runtime module
	const CompilerModule *compiler_module = vm->compiler_modules + i_compiler_module;
	uint32_t runtime_modules_length = vm->runtime_modules_length;
	uint32_t i_runtime_module = 0;
	for (; i_runtime_module < runtime_modules_length; ++i_runtime_module) {
		Module *runtime_module = vm->runtime_modules + i_runtime_module;
		if (sv_equals(compiler_module->name, runtime_module->name)) {
			break;
		}
	}
	if (i_runtime_module >= runtime_modules_length) {
		if (runtime_modules_length >= ARRAY_LENGTH(vm->runtime_modules)) {
			INIT_ERROR(&result.error, ErrorCode_Fatal);
			return result;
		}
		i_runtime_module = runtime_modules_length;
		vm->runtime_modules_length += 1;
	}

	// Fill module data
	Module *runtime_module = vm->runtime_modules + i_runtime_module;
	runtime_module->name = compiler_module->name;
	// Copy constants
	_Static_assert(sizeof(compiler_module->constants) == sizeof(runtime_module->constants));
	memcpy(runtime_module->constants, compiler_module->constants, sizeof(compiler_module->constants));
	// Copy bytecode
	uint32_t bytecode_len = compiler_module->bytecode_length > BYTECODE_LENGTH
				? BYTECODE_LENGTH
				: compiler_module->bytecode_length;
	memcpy(runtime_module->bytecode, compiler_module->bytecode, bytecode_len);
	runtime_module->bytecode_len = bytecode_len;
	// Fill import data
	const uint32_t max_length = ARRAY_LENGTH(runtime_module->import_addresses);
	uint32_t f = 0;
	for (; f < compiler_module->imported_functions_length && f < max_length; ++f) {
		uint32_t i_external_module = compiler_module->imported_module_indices[f];
		const CompilerModule *external_module = vm->compiler_modules + i_external_module;
		const Function *external_function = external_module->functions + compiler_module->imported_function_indices[f];

		runtime_module->import_module_names[f] = external_module->name;
		runtime_module->import_names[f] = external_function->name;
	
		runtime_module->import_module[f] = 0;
		runtime_module->import_addresses[f] = 0;
	}
	runtime_module->import_length = f;
	// Fill export data
	uint32_t export_length = 0;
	for (uint32_t e = 0; e < compiler_module->functions_length && e < max_length; ++e) {
		const Function *function = compiler_module->functions + e;
		if (function->type == FunctionType_Global) {
			runtime_module->export_names[export_length] = function->name;
			runtime_module->export_addresses[export_length] = function->address;
			export_length += 1;
		}
	}
	runtime_module->export_length = export_length;
	// Fill foreign function data
	for (f = 0; f < compiler_module->foreign_functions_length; ++f) {
		sv foreign_module_name = compiler_module->foreign_functions_module_name[f];
		sv foreign_function_name = compiler_module->foreign_functions_name[f];
		ForeignFn callback = vm->config.foreign_callback(foreign_module_name, foreign_function_name);
		if (callback == nullptr) {
			__debugbreak();
		}
		runtime_module->foreign_function_module_names[f] = foreign_module_name;
		runtime_module->foreign_function_names[f] = foreign_function_name;
		runtime_module->foreign_function_callback[f] = callback;
	}
	runtime_module->foreign_function_length = compiler_module->foreign_functions_length;
	
	// Link to other modules
	link_runtime_module(vm, i_runtime_module);
	result.i_runtime_module = i_runtime_module;
	return result;
}

CompileLoadLinkResult compile_load_link_code(VM *vm, sv module_name, sv code)
{
	CompileLoadLinkResult result = {0};
	CompilationResult comp_result = compile_code(vm, module_name, code);
	if (comp_result.error.code != ErrorCode_Ok) {
		result.error = comp_result.error;
		return result;
	}

	LoadModuleResult load_result = load_compiler_module(vm, comp_result.i_compiler_module);
	if (load_result.error.code != ErrorCode_Ok){
		result.error = load_result.error;
		return result;
	}
	result.i_compiler_module = comp_result.i_compiler_module;
	result.i_runtime_module = load_result.i_runtime_module;
	return result;
}

Error vm_compile(VM *vm, sv module_name, sv code)
{
	CompileLoadLinkResult result = compile_load_link_code(vm, module_name, code);
	return result.error;
}

void vm_call(VM *vm, sv module_name, sv function_name, Arena temp_mem)
{	
	char *log_buffer = (char*)arena_alloc(&temp_mem, 64);
	
	// Find the function name in the compiler module
	const uint32_t modules_len = vm->compiler_modules_length;
	uint32_t i_module = 0;
	for (; i_module < modules_len; ++i_module) {
		if (sv_equals(vm->compiler_modules[i_module].name, module_name)) {
			break;
		}
	}
	if (i_module >= modules_len) {
		StringBuilder sb = string_builder_from_buffer(log_buffer, 64);
		string_builder_append_sv(&sb, SV("Compiler module \""));
		string_builder_append_sv(&sb, module_name);
		string_builder_append_sv(&sb, SV("\" not found\n"));
		cross_log(cross_stderr, string_builder_get_string(&sb));
		return;
	}
	CompilerModule *module = vm->compiler_modules + i_module;
	const uint32_t functions_len = module->functions_length;
	uint32_t i_entrypoint = 0;
	uint32_t entrypoint_address = 0;
	for (; i_entrypoint < functions_len; ++i_entrypoint) {
		if (sv_equals(module->functions[i_entrypoint].name, function_name)) {
			entrypoint_address = module->functions[i_entrypoint].address;
			break;
		}
	}
	if (i_entrypoint >= functions_len) {
		StringBuilder sb = string_builder_from_buffer(log_buffer, 64);
		string_builder_append_sv(&sb, SV("Function '"));
		string_builder_append_sv(&sb, function_name);
		string_builder_append_sv(&sb, SV("' not found\n"));
		cross_log(cross_stderr, string_builder_get_string(&sb));
		return;
	}

	// Find the corresponding runtime module
	const uint32_t runtime_modules_len = vm->runtime_modules_length;
	uint32_t i_runtime_module = 0;
	for (; i_runtime_module < runtime_modules_len; ++i_runtime_module) {
		if (sv_equals(vm->runtime_modules[i_runtime_module].name, module_name)) {
			break;
		}
	}
	if (i_runtime_module >= runtime_modules_len) {
		StringBuilder sb = string_builder_from_buffer(log_buffer, 64);
		string_builder_append_sv(&sb, SV("Runtime module \""));
		string_builder_append_sv(&sb, module_name);
		string_builder_append_sv(&sb, SV("\" not found\n"));
		cross_log(cross_stderr, string_builder_get_string(&sb));
		return;
	}
	
	ExecutionContext *exec = (ExecutionContext*)(arena_alloc(&temp_mem, sizeof(ExecutionContext)));
	exec->modules = vm->runtime_modules;
	exec->modules_len = vm->runtime_modules_length;
	call_function(exec, i_runtime_module, entrypoint_address, nullptr, 0);
}
