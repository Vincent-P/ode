#include "vm.h"
#include "core/core.h"
#include "compiler.h"
#include "debug.h"
#include "executor.h"
#include "lexer.h"
#include "opcodes.h"
#include "parser.h"

#define VM_LOG 0
#define VM_MEMORY_SIZE (1u << 20)

VM *vm_create(Arena *memory, VMConfig config)
{
	Arena vm_memory;
	vm_memory.begin = arena_alloc(memory, VM_MEMORY_SIZE);
	vm_memory.end = vm_memory.begin + VM_MEMORY_SIZE;

	VM *vm = (VM*)arena_alloc(&vm_memory, sizeof(VM));
	vm->memory = vm_memory;
	vm->config = config;

	const uint32_t MAX_INTERNED_CHARS = 64;
	vm->identifiers_pool.string_buffer_capacity = MAX_INTERNED_CHARS;
	vm->identifiers_pool.string_buffer = arena_alloc(&vm_memory, MAX_INTERNED_CHARS);

	const uint32_t MAX_INTERNED_STRINGS = 16;
	vm->identifiers_pool.capacity = MAX_INTERNED_STRINGS;
	vm->identifiers_pool.keys = arena_alloc(&vm_memory, MAX_INTERNED_STRINGS * sizeof(*vm->identifiers_pool.keys));
	vm->identifiers_pool.sv_offset = arena_alloc(&vm_memory, MAX_INTERNED_STRINGS * sizeof(*vm->identifiers_pool.sv_offset));
	vm->identifiers_pool.sv_length = arena_alloc(&vm_memory, MAX_INTERNED_STRINGS * sizeof(*vm->identifiers_pool.sv_length));
	
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

CompileLoadLinkResult compile_load_link_code(Arena temp_mem, VM *vm, sv module_name, sv code);

typedef struct ParseModuleResult
{
	enum Result
	{
		ParseModule_LexerError,
		ParseModule_ParserError,
		ParseModule_LoadingError,
		ParseModule_Ok
	};

	uint32_t result;
	StringId *dependencies;
	uint32_t dependency_count;
} ParseModuleResult;

static sv vm_get_identifier(VM *vm, StringId id)
{
		return string_pool_get(&vm->identifiers_pool, id);
}

static ParseModuleResult vm_parse_module(Arena *memory, VM *vm, sv module_name)
{
	ParseModuleResult result = {0};

	sv code = {0};
	ASSERT(vm->config.load_module != NULL);
	bool loading_success = vm->config.load_module(module_name, &code);
	if (!loading_success) {
		result.result = ParseModule_LoadingError;
		return result;
	}

	// Allocate temporary compilation unit
	CompilationUnit *compunit = arena_alloc(memory, sizeof(CompilationUnit));
	compunit->input = code;
	const uint32_t MAX_INTERNED_CHARS = 1024;
	compunit->string_pool.string_buffer_capacity = MAX_INTERNED_CHARS;
	compunit->string_pool.string_buffer = arena_alloc(memory, MAX_INTERNED_CHARS);
	const uint32_t MAX_INTERNED_STRINGS = 256;
	compunit->string_pool.capacity = MAX_INTERNED_STRINGS;
	compunit->string_pool.keys = arena_alloc(memory, MAX_INTERNED_STRINGS * sizeof(*compunit->string_pool.keys));
	compunit->string_pool.sv_offset = arena_alloc(memory, MAX_INTERNED_STRINGS * sizeof(*compunit->string_pool.sv_offset));
	compunit->string_pool.sv_length = arena_alloc(memory, MAX_INTERNED_STRINGS * sizeof(*compunit->string_pool.sv_length));
	// Lex tokens
	LexerResult lexer_result = lexer_scan(memory, &compunit->string_pool, code);
	compunit->tokens = lexer_result.tokens;
	if (lexer_result.success == false) {
		result.result = ParseModule_LexerError;
		return result;
	}
	// Parse tree
	ParserResult parser_result = parse_module(memory, lexer_result.tokens);
	if (parser_result.success == false) {
		result.result = ParseModule_ParserError;
		return result;		
	}
	compunit->nodes = parser_result.nodes;

	// -- Compile dependencies first
	ScanDepsResult dependencies = compiler_scan_dependencies(memory, compunit);
	if (compunit->error.code != ErrorCode_Ok) {
		result.result = ParseModule_ParserError;
		return result;
	}

	result.result = ParseModule_Ok;
	result.dependencies = arena_alloc_n(memory, sizeof(StringId), dependencies.count);
	result.dependency_count = dependencies.count;
	for (uint32_t i_dep = 0; i_dep < dependencies.count; ++i_dep) {
		// Move module names from the compunit string  pool to the vm identifier pool
		sv dep_module_name = string_pool_get(&compunit->string_pool, dependencies.names[i_dep]);
		result.dependencies[i_dep] = string_pool_intern(&vm->identifiers_pool, dep_module_name);
	}

	return result;
}

// Parse code, compile module.
static CompilationResult compile_code(Arena temp_mem, VM *vm, sv module_name, sv code)
{
#if VM_LOG > 0
	char logbuf[128] = {0};
#endif	
	CompilationResult result = {0};


	StringId module_id = string_pool_intern(&vm->identifiers_pool, module_name);

	Arena vm_temp_mem = vm->memory;

	CompilationUnit compunit = {0};
	compunit.input = code;

	const uint32_t MAX_INTERNED_CHARS = 1024;
	compunit.string_pool.string_buffer_capacity = MAX_INTERNED_CHARS;
	compunit.string_pool.string_buffer = arena_alloc(&temp_mem, MAX_INTERNED_CHARS);

	const uint32_t MAX_INTERNED_STRINGS = 256;
	compunit.string_pool.capacity = MAX_INTERNED_STRINGS;
	compunit.string_pool.keys = arena_alloc(&temp_mem, MAX_INTERNED_STRINGS * sizeof(*compunit.string_pool.keys));
	compunit.string_pool.sv_offset = arena_alloc(&temp_mem, MAX_INTERNED_STRINGS * sizeof(*compunit.string_pool.sv_offset));
	compunit.string_pool.sv_length = arena_alloc(&temp_mem, MAX_INTERNED_STRINGS * sizeof(*compunit.string_pool.sv_length));
	
	// -- Lex tokens
	LexerResult lexer_result = lexer_scan(&temp_mem, &compunit.string_pool, code);
	compunit.tokens = lexer_result.tokens;

	if (lexer_result.success == false) {
#if VM_LOG > 0
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		// <file>:<line>:0: error Lexer[] returned <errcode>
		string_builder_append_sv(&sb, compunit.error.file);
		string_builder_append_char(&sb, ':');
		string_builder_append_u64(&sb, (uint64_t)(0));
		string_builder_append_sv(&sb, SV(":0: error: Lexer[]\n"));
		// Error at: '<errorstr>'
		build_error_at(code, lexer_result.error_span, &sb);
		cross_log(cross_stderr, string_builder_get_string(&sb));
#endif	
		__debugbreak();
		result.error = compunit.error;
		return result;
	}

	
	// -- Parse tokens into a parse tree
	ParserResult parser_result = parse_module(&temp_mem, lexer_result.tokens);
	compunit.nodes = parser_result.nodes;
	if (parser_result.success == false) {
#if VM_LOG > 0
		uint32_t token_count = array_count(compunit.tokens);
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		// <file>:<line>:0: error Parser[] returned <errcode>
		string_builder_append_sv(&sb, compunit.error.file);
		string_builder_append_char(&sb, ':');
		string_builder_append_u64(&sb, (uint64_t)(compunit.error.line));
		string_builder_append_sv(&sb, SV(":0: error: Parser[token_length: "));
		string_builder_append_u64(&sb, (uint64_t)(token_count));
		string_builder_append_sv(&sb, SV(", i_current_token: "));
		string_builder_append_u64(&sb, (uint64_t)(parser.i_current_token));
		string_builder_append_sv(&sb, SV("] returned "));
		string_builder_append_sv(&sb, SV(ErrorCode_str[(uint32_t)(compunit.error.code)]));
		string_builder_append_sv(&sb, SV("'\n"));
		cross_log(cross_stderr, string_builder_get_string(&sb));
		build_error_at(code, compunit.error.span, &sb);
		cross_log(cross_stderr, string_builder_get_string(&sb));
		if (token_count > 0) {
			uint32_t i_last_token = parser.i_current_token < token_count ? parser.i_current_token : token_count - 1;
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
#endif
		result.error = compunit.error;
		return result;
	}

	// -- Compile dependencies first
	uint32_t dep_module_indices[16] = {0};
	ScanDepsResult dependencies = compiler_scan_dependencies(&vm_temp_mem, &compunit);
	if (compunit.error.code != ErrorCode_Ok) {
		result.error = compunit.error;
		return result;
	}
	ASSERT(dependencies.count < ARRAY_LENGTH(dep_module_indices));
	for (uint32_t i_dep = 0; i_dep < dependencies.count; ++i_dep) {
		sv dep_module_name = string_pool_get(&compunit.string_pool, dependencies.names[i_dep]);
		sv dep_input = {0};
		// Get the code from the load_module callback
		bool loading_success = vm->config.load_module(dep_module_name, &dep_input);
		if (!loading_success) {
			INIT_ERROR(&compunit.error, ErrorCode_Fatal);
			result.error = compunit.error;
			return result;
		}

		CompileLoadLinkResult dep_result = compile_load_link_code(temp_mem, vm, dep_module_name, dep_input);
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
	compiler.module.name = module_id;
	compiler.module.imports = dep_module_indices;
	compiler.module.imports_length = dependencies.count;
	compile_module(&compiler);
	if (compunit.error.code != ErrorCode_Ok) {
#if VM_LOG > 0
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
#endif
		result.error = compunit.error;
		return result;
	}

#if VM_LOG > 1
	cross_log(cross_stderr, SV("\nCompilation success:\n"));
	for (uint32_t i_import = 0; i_import < compiler.module.imported_functions_length; ++i_import)
	{
		uint32_t i_imported_module = compiler.module.imported_module_indices[i_import];
		uint32_t i_imported_function = compiler.module.imported_function_indices[i_import];
		CompilerModule *imported_module = compiler.vm->compiler_modules + i_imported_module;
		sv imported_module_name = string_pool_get(&vm->identifiers_pool, imported_module->name);
		sv imported_function_name = string_pool_get(&vm->identifiers_pool, imported_module->functions[i_imported_function].name);

		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		string_builder_append_sv(&sb, SV("imported function: "));
		string_builder_append_sv(&sb, imported_module_name);
		string_builder_append_char(&sb, ' ');
		string_builder_append_sv(&sb, imported_function_name);
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));
	}
	print_bytecode(compiler.module.bytecode, compiler.module.bytecode_length);
#endif

	// -- Copy the new module to the VM
	uint32_t i_module = 0;
	for (; i_module < vm->compiler_modules_length; ++i_module) {
		if (vm->compiler_modules[i_module].name.id == module_id.id) {
			break;
		}
	}
	if (i_module >= vm->compiler_modules_length) {
		if (i_module >= ARRAY_LENGTH(vm->compiler_modules)) {
#if VM_LOG > 0
			cross_log(cross_stderr, SV("Fatal: module capacity\n"));
#endif
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
					if (runtime_module->import_names[i_import].id == other_module->export_names[i_export].id) {
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
				if (other_module->import_module_names[i_import].id == runtime_module->name.id) {
					// Reset other module import to 0
					other_module->import_addresses[i_import] = 0;
				
					for (uint32_t i_export = 0; i_export < our_export_length; ++i_export) {	
						if (other_module->import_names[i_import].id == runtime_module->export_names[i_export].id) {
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
		if (compiler_module->name.id == runtime_module->name.id) {
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
	uint32_t bytecode_len = compiler_module->bytecode_length > BYTECODE_CAPACITY
				? BYTECODE_CAPACITY
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
		sv foreign_module_name = string_pool_get(&vm->identifiers_pool, compiler_module->foreign_functions_module_name[f]);
		sv foreign_function_name = string_pool_get(&vm->identifiers_pool, compiler_module->foreign_functions_name[f]);
		ForeignFn callback = vm->config.foreign_callback(foreign_module_name, foreign_function_name);
		if (callback == NULL) {
			__debugbreak();
		}
		runtime_module->foreign_function_module_names[f] = compiler_module->foreign_functions_module_name[f];
		runtime_module->foreign_function_names[f] = compiler_module->foreign_functions_name[f];
		runtime_module->foreign_function_callback[f] = callback;
	}
	runtime_module->foreign_function_length = compiler_module->foreign_functions_length;
	
	// Link to other modules
	link_runtime_module(vm, i_runtime_module);
	result.i_runtime_module = i_runtime_module;
	return result;
}

CompileLoadLinkResult compile_load_link_code(Arena temp_mem, VM *vm, sv module_name, sv code)
{
	CompileLoadLinkResult result = {0};
	CompilationResult comp_result = compile_code(temp_mem, vm, module_name, code);
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

	if (vm->config.on_module_compiled != NULL) {
		vm->config.on_module_compiled(module_name);
	}
	
	return result;
}

Error vm_compile(Arena temp_mem, VM *vm, sv module_name, sv code)
{
	CompileLoadLinkResult result = compile_load_link_code(temp_mem, vm, module_name, code);
	return result.error;
}

Error vm_call(VM *vm, sv module_name, sv function_name, Arena temp_mem)
{
	Error err = (struct Error){0};
	StringId module_id = string_pool_intern(&vm->identifiers_pool, module_name);
	StringId function_id = string_pool_intern(&vm->identifiers_pool, function_name);

	// Find the function name in the compiler module
	const uint32_t modules_len = vm->compiler_modules_length;
	uint32_t i_module = 0;
	for (; i_module < modules_len; ++i_module) {
		if (vm->compiler_modules[i_module].name.id == module_id.id) {
			break;
		}
	}
	if (i_module >= modules_len) {
		INIT_ERROR(&err, ErrorCode_ModuleNotFound);
		return err;
	}
	CompilerModule *module = vm->compiler_modules + i_module;
	const uint32_t functions_len = module->functions_length;
	uint32_t i_entrypoint = 0;
	uint32_t entrypoint_address = 0;
	for (; i_entrypoint < functions_len; ++i_entrypoint) {
		if (module->functions[i_entrypoint].name.id == function_id.id) {
			entrypoint_address = module->functions[i_entrypoint].address;
			break;
		}
	}
	if (i_entrypoint >= functions_len) {
		INIT_ERROR(&err, ErrorCode_FunctionNotFound);
		return err;
	}

	// Find the corresponding runtime module
	const uint32_t runtime_modules_len = vm->runtime_modules_length;
	uint32_t i_runtime_module = 0;
	for (; i_runtime_module < runtime_modules_len; ++i_runtime_module) {
		if (vm->runtime_modules[i_runtime_module].name.id == module_id.id) {
			break;
		}
	}
	if (i_runtime_module >= runtime_modules_len) {
		INIT_ERROR(&err, ErrorCode_ModuleNotFound);
		return err;
	}
	
	ExecutionContext *exec = (ExecutionContext*)(arena_alloc(&temp_mem, sizeof(ExecutionContext)));
	exec->modules = vm->runtime_modules;
	exec->modules_len = vm->runtime_modules_length;
	exec->heap = vm->config.heap;
	call_function(exec, i_runtime_module, entrypoint_address, NULL, 0);
	return err;
}
