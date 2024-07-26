#pragma once

#include "asm-ast.h"

#include <stdio.h>

extern void emitcode(FILE *out, AsmNode *prog);
