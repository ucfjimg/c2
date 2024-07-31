#pragma once

#include "ast.h"
#include "list.h"
#include "symtab.h"
#include "tacnode.h"

extern TacNode *tcg_gen(AstProgram *prog, SymbolTable *stab);
