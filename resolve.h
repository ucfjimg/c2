#pragma once

#include "ast.h"

//
// Walk an AST and
// - ensure that variable usage and declarations are semantically ok
//   - variables are declared before use
//   - variables are not declared more than once in the same scope
// - rename variables to unique names within the entire program
//
extern void ast_resolve(AstNode *ast);
