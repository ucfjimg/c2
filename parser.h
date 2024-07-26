#pragma once

#include "ast.h"
#include "lexer.h"

extern AstProgram *parser_parse(Lexer *lex);
