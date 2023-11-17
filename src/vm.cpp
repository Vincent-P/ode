#include "vm.h"
#include "compiler.h"
#include "debug.h"
#include "executor.h"
#include "lexer.h"
#include "opcodes.h"
#include "parser.h"

#include <stdio.h>

VM *vm_create(VMConfig config)
{
	void *memory = calloc(1, sizeof(VM));
	VM *vm = static_cast<VM *>(memory);
	vm->config = config;
	vm->compiler_modules = vec_init<CompilerModule>(8);
	vm->runtime_modules = vec_init<Module>(8);
	return vm;
}

void vm_destroy(VM *vm)
{
	free(vm);
}

static CompilerModule module_alloc(sv module_name)
{
	CompilerModule new_module = {};
	const uint32_t functions_capacity = 16;
	new_module.functions_capacity = functions_capacity;
	new_module.functions = static_cast<Function *>(calloc(functions_capacity, sizeof(Function)));

	const uint32_t bytecode_capacity = 1024;
	new_module.bytecode_capacity = bytecode_capacity;
	new_module.bytecode = static_cast<uint8_t *>(calloc(bytecode_capacity, sizeof(OpCode)));

	const uint32_t types_capacity = 8;
	new_module.types_capacity = types_capacity;
	new_module.types = static_cast<UserDefinedType *>(calloc(types_capacity, sizeof(UserDefinedType)));

	const uint32_t imported_capacity = 8;
	new_module.imported_module_indices = static_cast<uint32_t *>(calloc(imported_capacity, sizeof(uint32_t)));
	new_module.imported_function_indices = static_cast<uint32_t *>(calloc(imported_capacity, sizeof(uint32_t)));
	new_module.imported_functions_length = 0;

	new_module.name = module_name;
	return new_module;
}

struct CompilationResult
{
	Error error;
	uint32_t i_compiler_module;
};

struct LoadModuleResult
{
	Error error;
	uint32_t i_runtime_module;
};

struct CompileLoadLinkResult
{
	Error error;
	uint32_t i_compiler_module;
	uint32_t i_runtime_module;
};

CompileLoadLinkResult compile_load_link_code(VM *vm, sv module_name, sv code);

// Parse code, compile module.
static CompilationResult compile_code(VM *vm, sv module_name, sv code)
{
	CompilationResult result = {};

	CompilationUnit compunit = {};
	compunit.input = code;
	compunit.tokens = vec_init<Token>(4096);
	compunit.nodes = vec_init<AstNode>(4096);
	
	// -- Lex tokens
	lexer_scan(&compunit);
	if (compunit.error.code != ErrorCode::LexerDone) {
		fprintf(stderr, "# Lexer returned %s\n", ErrorCode_str[uint32_t(compunit.error.code)]);
		sv error_str = sv_substr(code, compunit.error.span);
		fprintf(stderr, "Error at: '%.*s'\n", int(error_str.length), error_str.chars);
		result.error = compunit.error;
		return result;
	}
	compunit.error.code = ErrorCode::Ok;	// Reset the result to Ok. TODO: remove lexerdone?
	
	// -- Parse tokens into a parse tree
	Parser parser = {};
	parser.compunit = &compunit;
	parse_module(&parser);
	if (compunit.error.code != ErrorCode::Ok) {
		fprintf(stderr,
			"# Parser[token_length: %u, i_current_token: %u] returned %s\n",
			compunit.tokens.length,
			parser.i_current_token,
			ErrorCode_str[uint32_t(compunit.error.code)]);
		sv error_str = sv_substr(code, compunit.error.span);
		fprintf(stderr, "Error at: '%.*s'\n", int(error_str.length), error_str.chars);
		if (compunit.tokens.length > 0) {
			uint32_t i_last_token =
				parser.i_current_token < compunit.tokens.length ? parser.i_current_token : compunit.tokens.length - 1;
			const Token *last_token = vec_at(&compunit.tokens, i_last_token);
			sv last_token_str = sv_substr(compunit.input, last_token->span);

			const char *token_kind_str = TokenKind_str[uint32_t(last_token->kind)];

			fprintf(stderr,
				"# Last seen token is %s[%.*s]\n",
				token_kind_str,
				int(last_token_str.length),
				last_token_str.chars);
		}
		if (parser.expected_token_kind != TokenKind::Invalid) {
			fprintf(stderr, "# Expected token of kind %s\n", TokenKind_str[uint32_t(parser.expected_token_kind)]);
		}
		result.error = compunit.error;
		return result;
	}

	// -- Compile dependencies first
	const Token *require_paths[16] = {};
	uint32_t dep_module_indices[16] = {};
	uint32_t require_paths_length = 0;
	compiler_scan_requires(&compunit, require_paths, 16, &require_paths_length);
	if (require_paths_length == 16) {
		INIT_ERROR(&compunit.error, ErrorCode::Fatal);
		fprintf(stderr, "Too much requires!\n");
	}
	if (compunit.error.code != ErrorCode::Ok) {
		Error err = compunit.error;
		fprintf(stderr,
			"%s:%d:0: error: scan_requires[] returned %s\n",
			err.file.chars,
			err.line,
			ErrorCode_str[uint32_t(err.code)]);
		result.error = compunit.error;
		return result;
	}
	for (uint32_t i_dep = 0; i_dep < require_paths_length; ++i_dep) {
		sv dep_module_name = sv_substr(code, require_paths[i_dep]->span);
		// HACK: Because string literals are substring of the code (and not properly interned), we have double quotes
		dep_module_name.chars += 1;
		dep_module_name.length -= 2;
		sv dep_input = {};
		// Get the code from the load_module callback
		bool loading_success = vm->config.load_module(dep_module_name, &dep_input);
		if (!loading_success) {
			INIT_ERROR(&compunit.error, ErrorCode::Fatal);
			result.error = compunit.error;
			return result;
		}

		CompileLoadLinkResult dep_result = compile_load_link_code(vm, dep_module_name, dep_input);
		if (dep_result.error.code != ErrorCode::Ok) {
			result.error = dep_result.error;
			return result;
		}
		dep_module_indices[i_dep] = dep_result.i_compiler_module;
	}
	
	// -- Compile the parse tree into bytecode
	Compiler compiler = {};
	compiler.vm = vm;
	compiler.compunit = &compunit;
	compiler.scopes = vec_init<LexicalScope>(16);
	compiler.module = module_alloc(module_name);
	compiler.module.imports = dep_module_indices;
	compiler.module.imports_length = require_paths_length;
	compile_module(&compiler);
	if (compunit.error.code != ErrorCode::Ok) {
		Error err = compunit.error;
		fprintf(stderr,
			"%s:%d:0: error: Compiler[] returned %s\n",
			err.file.chars,
			err.line,
			ErrorCode_str[uint32_t(err.code)]);
		sv error_str = sv_substr(code, err.span);
		fprintf(stderr, "Error at: '%.*s'\n", int(error_str.length), error_str.chars);
		const char *expected_type_str = "(struct)";
		if (!type_id_is_user_defined(err.expected_type)) {
			expected_type_str = BuiltinTypeKind_str[uint32_t(err.expected_type.builtin.kind)];
		}
		fprintf(stderr, "# expected type #%u %s\n", err.expected_type.raw, expected_type_str);
		const char *got_type_str = "(struct)";
		if (!type_id_is_user_defined(err.got_type)) {
			got_type_str = BuiltinTypeKind_str[uint32_t(err.got_type.builtin.kind)];
		}
		fprintf(stderr, "# got type #%u %s\n", err.got_type.raw, got_type_str);
		result.error = compunit.error;
		return result;
	}
	fprintf(stdout, "\nCompilation success:\n");
	for (uint32_t i_import = 0; i_import < compiler.module.imported_functions_length; ++i_import)
	{
		uint32_t i_imported_module = compiler.module.imported_module_indices[i_import];
		uint32_t i_imported_function = compiler.module.imported_function_indices[i_import];
		CompilerModule *imported_module = vec_at(&compiler.vm->compiler_modules, i_imported_module);
		sv imported_module_name = imported_module->name;
		sv imported_function_name = imported_module->functions[i_imported_function].name;
		fprintf(stdout, "imported function: %.*s %.*s\n",
			int(imported_module_name.length), imported_module_name.chars,
			int(imported_function_name.length), imported_function_name.chars);
	}
	print_bytecode(compiler.module.bytecode, compiler.module.bytecode_length);

	// -- Copy the new module to the VM
	uint32_t i_module = 0;
	for (; i_module < vm->compiler_modules.length; ++i_module) {
		CompilerModule *module = vec_at(&vm->compiler_modules, i_module);
		if (sv_equals(module->name, module_name)) {
			break;
		}
	}
	if (i_module >= vm->compiler_modules.length) {
		if (i_module >= vm->compiler_modules.capacity) {
			fprintf(stdout, "Fatal: module capacity\n");
			INIT_ERROR(&compunit.error, ErrorCode::Fatal);
			result.error = compunit.error;
			return result;
		}
		i_module = vec_append(&vm->compiler_modules, compiler.module);
	}
	else {
		*vec_at(&vm->compiler_modules, i_module) = compiler.module;
	}
	
	result.i_compiler_module =  i_module;
	return result;
}

// This function could be used to load runtime modules from disk!
static void link_runtime_module(VM *vm, uint32_t i_runtime_module)
{
	// Link with other runtime modules, checking if they import a function from the new runtime module.
	Module *runtime_module = vec_at(&vm->runtime_modules, i_runtime_module);
	const uint32_t our_import_length = runtime_module->import_length;
	const uint32_t our_export_length = runtime_module->export_length;

	uint32_t runtime_modules_length = vm->runtime_modules.length;
	for (uint32_t m = 0; m < runtime_modules_length; ++m) {
		if (m != i_runtime_module) {
			Module *other_module = vec_at(&vm->runtime_modules, m);
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
	LoadModuleResult result = {};

	// Find or create runtime module
	const CompilerModule *compiler_module = vec_at(&vm->compiler_modules, i_compiler_module);
	uint32_t runtime_modules_length = vm->runtime_modules.length;
	uint32_t i_runtime_module = 0;
	for (; i_runtime_module < runtime_modules_length; ++i_runtime_module) {
		Module *runtime_module = vec_at(&vm->runtime_modules, i_runtime_module);
		if (sv_equals(compiler_module->name, runtime_module->name)) {
			break;
		}
	}
	if (i_runtime_module >= runtime_modules_length) {
		if (runtime_modules_length >= vm->runtime_modules.capacity) {
			INIT_ERROR(&result.error, ErrorCode::Fatal);
			return result;
		}
		i_runtime_module = vec_append(&vm->runtime_modules, {});
	}

	// Fill module data
	Module *runtime_module = vec_at(&vm->runtime_modules, i_runtime_module);
	runtime_module->name = compiler_module->name;
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
		const CompilerModule *external_module = vec_at(&vm->compiler_modules, i_external_module);
		Function *external_function = external_module->functions + compiler_module->imported_function_indices[f];
		runtime_module->import_module_names[f] = external_module->name;
		runtime_module->import_names[f] = external_function->name;
		
		runtime_module->import_module[f] = 0;
		runtime_module->import_addresses[f] = 0;
	}
	runtime_module->import_length = f;
	// Fill export data
	uint32_t export_length = 0;
	for (uint32_t e = 0; e < compiler_module->functions_length && e < max_length; ++e) {
		Function *function = compiler_module->functions + e;
		if (function->type == FunctionType::Global) {
			runtime_module->export_names[export_length] = function->name;
			runtime_module->export_addresses[export_length] = function->address;
			export_length += 1;
		}
	}
	runtime_module->export_length = export_length;
	
	// Link to other modules
	link_runtime_module(vm, i_runtime_module);
	result.i_runtime_module = i_runtime_module;
	return result;
}

CompileLoadLinkResult compile_load_link_code(VM *vm, sv module_name, sv code)
{
	CompileLoadLinkResult result = {};
	CompilationResult comp_result = compile_code(vm, module_name, code);
	if (comp_result.error.code != ErrorCode::Ok) {
		result.error = comp_result.error;
		return result;
	}

	LoadModuleResult load_result = load_compiler_module(vm, comp_result.i_compiler_module);
	if (load_result.error.code != ErrorCode::Ok){
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

void vm_call(VM *vm, sv module_name, sv function_name)
{	
	// Find the function name in the compiler module
	const uint32_t modules_len = vm->compiler_modules.length;
	uint32_t i_module = 0;
	for (; i_module < modules_len; ++i_module) {
		if (sv_equals(vec_at(&vm->compiler_modules, i_module)->name, module_name)) {
			break;
		}
	}
	if (i_module >= modules_len) {
		fprintf(stderr, "Compiler module \"%.*s\" not found\n", int(module_name.length), module_name.chars);
		return;
	}
	CompilerModule *module = vec_at(&vm->compiler_modules, i_module);
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
		fprintf(stderr, "main not found\n");
		return;
	}

	// Find the corresponding runtime module
	const uint32_t runtime_modules_len = vm->runtime_modules.length;
	uint32_t i_runtime_module = 0;
	for (; i_runtime_module < runtime_modules_len; ++i_runtime_module) {
		if (sv_equals(vec_at(&vm->runtime_modules, i_runtime_module)->name, module_name)) {
			break;
		}
	}
	if (i_runtime_module >= runtime_modules_len) {
		fprintf(stderr, "Runtime module \"%.*s\" not found\n", int(module_name.length), module_name.chars);
		return;
	}
	
	ExecutionContext *exec = static_cast<ExecutionContext *>(calloc(1, sizeof(ExecutionContext)));
	exec->modules = vm->runtime_modules.data;
	exec->modules_len = vm->runtime_modules.length;
	call_function(exec, i_runtime_module, entrypoint_address, nullptr, 0);

	free(exec);
	exec = nullptr;
}
