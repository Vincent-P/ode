#include "vm.h"
#include "compiler.h"
#include "executor.h"
#include "debug.h"

#include <stdio.h>


VM *vm_create(VMConfig config)
{
	void *memory = calloc(1, sizeof(VM));
	VM* vm = static_cast<VM*>(memory);
	vm->config = config;
	return vm;
}

void vm_destroy(VM *vm)
{
	free(vm);
}

static void module_init(Module *new_module, sv module_name)
{
	const uint32_t functions_capacity = 16;
	new_module->functions_capacity = functions_capacity;
	new_module->functions = static_cast<Function *>(calloc(functions_capacity, sizeof(Function)));

	const uint32_t bytecode_capacity = 1024;
	new_module->bytecode_capacity = bytecode_capacity;
	new_module->bytecode = static_cast<uint8_t *>(calloc(bytecode_capacity, sizeof(OpCodeKind)));

	const uint32_t types_capacity = 8;
	new_module->types_capacity = types_capacity;
	new_module->types = static_cast<UserDefinedType *>(calloc(types_capacity, sizeof(UserDefinedType)));

	const uint32_t constants_capacity = 16;
	new_module->constant_strings_capacity = constants_capacity;
	new_module->constant_strings = static_cast<sv *>(calloc(constants_capacity, sizeof(sv)));
	new_module->constants_u32_capacity = constants_capacity;
	new_module->constants_u32 = static_cast<uint32_t *>(calloc(constants_capacity, sizeof(uint32_t)));

	new_module->name = module_name;
}

void vm_compile(VM* vm, sv module_name, sv code)
{
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
		return;
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
		return;
	}
	// -- Compile the parse tree into bytecode
	Module new_module = {};
	module_init(&new_module, module_name);
	Compiler compiler = {};
	compiler.compunit = &compunit;
	compiler.current_module = &new_module;
	compiler.scopes = vec_init<LexicalScope>(16);
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
		fprintf(stderr, "%s:%d:0: error: Compiler[] returned %s\n", err.file.chars, err.line, ErrorCode_str[uint32_t(err.code)]);
		return;
	}
	// Compile dependencies
	for (uint32_t i_dep = 0; i_dep < require_paths_length; ++i_dep){
		sv dep_module_name = sv_substr(code, require_paths[i_dep]->span);
		// HACK: Because string literals are substring of the code (and not properly interned), we have double quotes
		dep_module_name.chars += 1;
		dep_module_name.length -= 2;
		sv dep_input = {};
		bool loading_success = vm->config.load_module(dep_module_name, &dep_input);
		if (!loading_success) {
			INIT_ERROR(&compunit.error, ErrorCode::Fatal);
			return;
		}
		vm_compile(vm, dep_module_name, dep_input);
		// TODO: Error handling for vm_compile
		// dep_module has been compiled
	}
	// Finally compile ourselves
	compile_module(&compiler);
	if (compunit.error.code != ErrorCode::Ok) {
		Error err = compunit.error;
		fprintf(stderr, "%s:%d:0: error: Compiler[] returned %s\n", err.file.chars, err.line, ErrorCode_str[uint32_t(err.code)]);
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
		return;
	}
	fprintf(stdout, "\nCompilation success:\n");
	fprintf(stdout, "Exported types: %u\n", new_module.types_length);
	print_bytecode(new_module.bytecode, new_module.bytecode_length);
	// Everything went well, copy the new compiled module to the persistant compiler state
	// Find module
	uint32_t i_module = 0;
	for (; i_module < vm->modules.length; ++i_module) {
		Module *module = vec_at(&vm->modules, i_module);
		if (sv_equals(module->name, module_name)) {
			break;
		}
	}
	// Not found, create a new module
	if (i_module >= vm->modules.length) {
		if (i_module >= vm->modules.capacity) {
			INIT_ERROR(&compunit.error, ErrorCode::Fatal);
			return;
		}

		i_module = vm->modules.length;
		vec_append(&vm->modules, {});
	}
	Module *compiler_module = vec_at(&vm->modules, i_module);
	// TODO: free previous module?
	*compiler_module = new_module;
	// *_module = vec_at(&vm->modules, i_module);
}

void vm_interpret(VM* vm, sv module_name, sv code)
{
	vm_compile(vm, module_name, code);

	// Find the module
	const uint32_t modules_len = vm->modules.length;
	uint32_t i_module = 0;
	for (; i_module < modules_len; ++i_module) {
		if (sv_equals(vec_at(&vm->modules, i_module)->name, module_name)) {
			break;
		}
	}
	if (i_module >= modules_len) {
		// Module not found
		fprintf(stderr, "module \"%.*s\" not found\n", int(module_name.length), module_name.chars);
		return;
	}
	// Find main function
	Module *module = vec_at(&vm->modules, i_module);
	const uint32_t functions_len = module->functions_length;
	const sv entrypoint_name = sv_from_null_terminated("main");
	uint32_t i_entrypoint = 0;
	for (; i_entrypoint < functions_len; ++i_entrypoint) {
		if (sv_equals(module->functions[i_entrypoint].name, entrypoint_name)) {
			break;
		}
	}
	if (i_entrypoint >= functions_len) {
		// Entrypoint not found
		fprintf(stderr, "main not found\n");
		return;
	}

	const Function *entrypoint = module->functions + i_entrypoint;
	uint32_t ip = entrypoint->address;
	executor_execute_module_at(vm, i_module, ip);
}
