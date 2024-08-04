#pragma once

#include "tacnode.h"
#include "asm-ast.h"
#include "ast.h"
#include "symtab.h"

extern AsmNode *codegen(struct TacNode *taccode, SymbolTable *stab);
