#pragma once

#include "ast.h"
#include "lexer.h"

extern AstProgram *parser_parse(AstState *ast_state, Lexer *lex);
