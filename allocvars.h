#pragma once

#include "asm-ast.h"
#include "symtab.h"

//
// Code generation pass to allocate local variables in the stack frame
// and replace pseudo-register operands with stack frame offsets.
//
extern void asm_allocate_vars(AsmNode *prog, SymbolTable *stab);
