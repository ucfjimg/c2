#pragma once

#include "ast.h"
#include "symtab.h"

//
// Type check the program.
//
extern void ast_typecheck(AstProgram *prog, SymbolTable *stab, AstState *ast);
