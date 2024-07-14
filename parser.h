#pragma once

#include "ast.h"
#include "lexer.h"

extern AstNode *parser_parse(Lexer *lex);
