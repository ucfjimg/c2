#pragma once

#include "ast.h"
#include "list.h"
#include "symtab.h"
#include "tacnode.h"
#include "typetab.h"

extern TacNode *tcg_gen(AstProgram *prog, SymbolTable *stab, TypeTable *typetab);
