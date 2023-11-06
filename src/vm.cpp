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
	return vm;
}

void vm_destroy(VM *vm)
{
	free(vm);
}

static void module_init(CompilerModule *new_module, sv module_name)
{
	const uint32_t functions_capacity = 16;
	new_module->functions_capacity = functions_capacity;
	new_module->functions = static_cast<Function *>(calloc(functions_capacity, sizeof(Function)));

	const uint32_t bytecode_capacity = 1024;
	new_module->bytecode_capacity = bytecode_capacity;
	new_module->bytecode = static_cast<uint8_t *>(calloc(bytecode_capacity, sizeof(OpCode)));

	const uint32_t types_capacity = 8;
	new_module->types_capacity = types_capacity;
	new_module->types = static_cast<UserDefinedType *>(calloc(types_capacity, sizeof(UserDefinedType)));

	const uint32_t imports_capacity = 8;
	new_module->imports_capacity = imports_capacity;
	new_module->imports = static_cast<uint32_t *>(calloc(imports_capacity, sizeof(uint32_t)));

	new_module->name = module_name;
}

Error vm_compile(VM *vm, sv module_name, sv code)
{
	// -- Find module
	uint32_t i_module = 0;
	for (; i_module < vm->compiler_modules.length; ++i_module) {
		CompilerModule *module = vec_at(&vm->compiler_modules, i_module);
		if (sv_equals(module->name, module_name)) {
			break;
		}
	}
	if (i_module < vm->compiler_modules.length) {
		// CompilerModule has been found
	} else {
		// First compilation
	}
	// -- Create compilation unit data
	CompilationUnit compunit = {};
	compunit.input = code;
	compunit.tokens = vec_init<Token>(4096);
	compunit.nodes = vec_init<AstNode>(4096);
	// -- Lex tokens
	// Generate tokens
	lexer_scan(&compunit);
	if (compunit.error.code != ErrorCode::LexerDone) {
		fprintf(stderr, "# Lexer returned %s\n", ErrorCode_str[uint32_t(compunit.error.code)]);
		sv error_str = sv_substr(code, compunit.error.span);
		fprintf(stderr, "Error at: '%.*s'\n", int(error_str.length), error_str.chars);
		return compunit.error;
	}
	// Reset the result to Ok from LexerDone
	// TODO: remove lexerdone?
	compunit.error.code = ErrorCode::Ok;
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
		return compunit.error;
	}
	// -- Compile the parse tree into bytecode
	Compiler compiler = {};
	compiler.vm = vm;
	compiler.compunit = &compunit;
	compiler.scopes = vec_init<LexicalScope>(16);
	compiler.i_previous_module = ~uint32_t(0);
	if (i_module < compiler.vm->compiler_modules.length) {
		compiler.i_previous_module = i_module;
	}
	module_init(&compiler.module, module_name);
	// Scan for module dependencies
	const Token *require_paths[16] = {};
	uint32_t require_paths_length = 0;
	compiler_scan_requires(&compiler, require_paths, 16, &require_paths_length);
	if (require_paths_length == 16) {
		INIT_ERROR(&compunit.error, ErrorCode::Fatal);
		fprintf(stderr, "Too much requires!\n");
	}
	if (compunit.error.code != ErrorCode::Ok) {
		Error err = compunit.error;
		fprintf(stderr,
			"%s:%d:0: error: Compiler[] returned %s\n",
			err.file.chars,
			err.line,
			ErrorCode_str[uint32_t(err.code)]);
		return compunit.error;
	}
	// Compile dependencies
	for (uint32_t i_dep = 0; i_dep < require_paths_length; ++i_dep) {
		sv dep_module_name = sv_substr(code, require_paths[i_dep]->span);
		// HACK: Because string literals are substring of the code (and not properly interned), we have double quotes
		dep_module_name.chars += 1;
		dep_module_name.length -= 2;
		sv dep_input = {};
		bool loading_success = vm->config.load_module(dep_module_name, &dep_input);
		if (!loading_success) {
			INIT_ERROR(&compunit.error, ErrorCode::Fatal);
			return compunit.error;
		}
		Error dep_error = vm_compile(vm, dep_module_name, dep_input);
		if (dep_error.code != ErrorCode::Ok) {
			return dep_error;
		}
		// dep_module has been compiled
		// TODO: return the index of the compiled module.
		for (uint32_t i = 0; i < vm->compiler_modules.length; ++i) {
			CompilerModule *m = vec_at(&vm->compiler_modules, i);
			if (sv_equals(m->name, dep_module_name)) {
				if (compiler.module.imports_length >= compiler.module.imports_capacity) {
					INIT_ERROR(&compunit.error, ErrorCode::Fatal);
					return compunit.error;
				}
				compiler.module.imports[compiler.module.imports_length] = i;
				compiler.module.imports_length += 1;
				break;
			}
		}
	}
	// Finally compile ourselves
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
		return compunit.error;
	}
	fprintf(stdout, "\nCompilation success:\n");
	fprintf(stdout, "Exported types: %u\n", compiler.module.types_length);
	print_bytecode(compiler.module.bytecode, compiler.module.bytecode_length);
	// Everything went well, copy the new compiled module to the persistant compiler state
	// Find module
	for (; i_module < vm->compiler_modules.length; ++i_module) {
		CompilerModule *module = vec_at(&vm->compiler_modules, i_module);
		if (sv_equals(module->name, module_name)) {
			break;
		}
	}
	// Not found, create a new module
	if (i_module >= vm->compiler_modules.length) {
		if (i_module >= vm->compiler_modules.capacity) {
			fprintf(stdout, "Fatal: module capacity\n");
			INIT_ERROR(&compunit.error, ErrorCode::Fatal);
			return compunit.error;
		}

		i_module = vm->compiler_modules.length;
		vec_append(&vm->compiler_modules, {});
	}
	CompilerModule *compiler_module = vec_at(&vm->compiler_modules, i_module);
	*compiler_module = compiler.module;
	return {};
}

void vm_call(VM *vm, sv module_name, sv function_name)
{
	// Find the module
	const uint32_t modules_len = vm->compiler_modules.length;
	uint32_t i_module = 0;
	for (; i_module < modules_len; ++i_module) {
		if (sv_equals(vec_at(&vm->compiler_modules, i_module)->name, module_name)) {
			break;
		}
	}
	if (i_module >= modules_len) {
		// CompilerModule not found
		fprintf(stderr, "module \"%.*s\" not found\n", int(module_name.length), module_name.chars);
		return;
	}
	// Find main function
	CompilerModule *module = vec_at(&vm->compiler_modules, i_module);
	const uint32_t functions_len = module->functions_length;
	uint32_t i_entrypoint = 0;
	for (; i_entrypoint < functions_len; ++i_entrypoint) {
		if (sv_equals(module->functions[i_entrypoint].name, function_name)) {
			break;
		}
	}
	if (i_entrypoint >= functions_len) {
		// Entrypoint not found
		fprintf(stderr, "main not found\n");
		return;
	}

	ExecutionContext *exec = static_cast<ExecutionContext *>(calloc(1, sizeof(ExecutionContext)));

	// Copy bytecode from compiler module to executor module
	for (uint32_t i = 0; i < modules_len; ++i) {
		CompilerModule *m = vec_at(&vm->compiler_modules, i);

		uint32_t bytecode_len = m->bytecode_length > BYTECODE_LENGTH ? BYTECODE_LENGTH : m->bytecode_length;
		memcpy(exec->modules[i].bytecode, m->bytecode, bytecode_len);
		exec->modules[i].bytecode_len = bytecode_len;

		uint32_t max_length = ARRAY_LENGTH(exec->modules[i].function_addresses);
		for (uint32_t f = 0; f < m->functions_length && f < max_length; ++f) {
			exec->modules[i].function_addresses[f] = m->functions[f].address;
		}
	}
	exec->modules_len = modules_len;
	call_function(exec, i_module, i_entrypoint, nullptr, 0);

	free(exec);
	exec = nullptr;
	// Instanciate the module
	// No more space to add the module!
	// if (i_module >= vm->module_instances.capacity) {
	// 	return;
	// }
	// The module was not found. Add it.
	// if (i_module == vm->module_instances.length) {
	// 	vm->module_instances.length += 1;
	// }
	// ModuleInstance *module_instance = vec_at(&vm->module_instances, i_module);
	// if (module_instance->functions_capacity == 0) {
	// 	module_instance->functions_capacity = 8;
	// 	module_instance->functions =
	// 		static_cast<FunctionInstance *>(calloc(module_instance->functions_capacity, sizeof(FunctionInstance)));
	// } else {
	// 	memset(module_instance->functions, 0, sizeof(FunctionInstance) * module_instance->functions_capacity);
	// }
	// module_instance->functions_length = module->functions_length;
	// // Find foreign functions
	// for (uint32_t i_function = 0; i_function < module->functions_length; ++i_function) {
	// 	if (module->functions[i_function].type == FunctionType::Foreign) {
	// 		const sv function_name = module->functions[i_function].name;
	// 		module_instance->functions[i_function].foreign = vm->config.foreign_callback(module_name, function_name);
	// 	}
	// }
	// Execute
	// const Function *entrypoint = module->functions + i_entrypoint;
	// uint32_t ip = entrypoint->address;
	// executor_execute_module_at(vm, i_module, ip);
}
