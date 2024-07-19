#pragma once

#include "asm-ast.h"

//
// Code generation pass to fix instructions which violate target
// operand constraints.
//
extern void asm_fix_operands(AsmNode *prog);
