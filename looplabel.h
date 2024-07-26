#pragma once

#include "ast.h"

//
// Walk the AST and label loops. Assign each loop a unique label.
// Assign each break or continue statement the label of the 
// innermost surrounding loop.
//
extern void ast_label_loops(AstProgram *prog);
