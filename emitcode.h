#pragma once

#include "asm-ast.h"
#include "backsym.h"

#include <stdio.h>

extern void emitcode(FILE *out, AsmNode *prog, BackEndSymbolTable *bstab);
