#pragma once

#include "ast.h"
#include "symtab.h"
#include "typetab.h"

//
// Type check the program.
//
extern void ast_typecheck(AstProgram *prog, SymbolTable *stab, TypeTable *typetab, AstState *ast);
