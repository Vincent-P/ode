#pragma once

/*
multiple "functions" in form of bytecode
instruction_banks = [MemoryBlock]
data = MemoryBlock

"calling" a function MAY results in a instruction_bank switch + jump
current_bank/ip per callframe?

compiler stitches all function (or module) bytecode into multiple banks, and execute
*/

struct ExecutorState;
void execute(ExecutorState *state);
