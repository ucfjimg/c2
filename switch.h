#pragma once

#include "ast.h"

//
// Walk through the AST and resolve case statements inside
// a switch. 
//
extern void ast_validate_switch(AstProgram *prog);
