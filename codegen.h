#pragma once

#include "asm-ast.h"
#include "ast.h"
#include "backsym.h"
#include "symtab.h"
#include "tacnode.h"
#include "typetab.h"

extern AsmNode *codegen(struct TacNode *taccode, SymbolTable *stab, TypeTable *typetab, BackEndSymbolTable *bstab);
extern void codegen_sym_to_backsym(SymbolTable *stab, BackEndSymbolTable *bstab, TypeTable *typetab);

