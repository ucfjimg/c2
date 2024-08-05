#pragma once

#include "asm-ast.h"
#include "ast.h"
#include "backsym.h"
#include "symtab.h"
#include "tacnode.h"

extern AsmNode *codegen(struct TacNode *taccode, SymbolTable *stab);
extern BackEndSymbolTable *codegen_sym_to_backsym(SymbolTable *stab);

