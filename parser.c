#include "parser.h"

#include "errors.h"
#include "safemem.h"

typedef struct {
    Lexer *lex;
    Token tok;
} Parser;

//
// Free the current token and parse the next one.
//
static void parse_next_token(Parser *parser)
{
    token_free(&parser->tok);
    lexer_token(parser->lex, &parser->tok);
}

//
// Report an error when a token was expected, but something else was
// found. `tok` is what was actually found (and will be used for location);
// `expected` is a free-form string of what was needed.
// 
static void report_expected_err(Token *tok, char *expected)
{
    char *tok_desc = token_describe(tok);
    err_report(EC_ERROR, &tok->loc, "expected %s; found `%s`.", expected, tok_desc);
    safe_free(tok_desc);
}

//
// Parse a unary operator.
//
static bool parse_unary_op(Parser *parser, UnaryOp *uop)
{
    switch (parser->tok.type) {
        case TOK_PLUS:          *uop = UOP_PLUS; break;
        case TOK_MINUS:         *uop = UOP_MINUS; break;
        case TOK_COMPLEMENT:    *uop = UOP_COMPLEMENT; break;

        default:
            return false;
    }

    parse_next_token(parser);
    return true;
}

//
// Parse an expression.
//
static Expression *parse_expression(Parser *parser)
{
    FileLine loc = parser->tok.loc;

    //
    // <expression> := <int> | <unop> <exp> | "(" <exp> ")"
    //
    if (parser->tok.type == TOK_INT_CONST) {
        Expression *exp = exp_int(parser->tok.intval);
        exp->loc = loc;
        parse_next_token(parser);
        return exp;
    }

    UnaryOp uop;
    if (parse_unary_op(parser, &uop)) {
        Expression *rhs = parse_expression(parser);
        Expression *exp = exp_unary(uop, rhs);
        exp->loc = loc;
        return exp;
    }

    if (parser->tok.type == '(') {
        parse_next_token(parser);
        Expression *exp = parse_expression(parser);
        exp->loc = loc;
        if (parser->tok.type == ')') {
            parse_next_token(parser);
        } else {
            report_expected_err(&parser->tok, ")");
        }
        return exp;
    }

    report_expected_err(&parser->tok, "constant, operator, or (");

    Expression *exp = exp_int(0);
    exp->loc = loc;
    return exp;
}

//
// Parse a statement.
//
static Statement *parse_statement(Parser *parser)
{
    FileLine loc = parser->tok.loc;

    //
    // <statement> := "return" <exp> ";"
    //
    if (parser->tok.type != TOK_RETURN) {
        report_expected_err(&parser->tok, "`return`");
        Statement *null = stmt_null();
        null->loc = loc;
        return null;
    }

    parse_next_token(parser);
    Expression *exp = parse_expression(parser);
    
    if (parser->tok.type != ';') {
        report_expected_err(&parser->tok, "`;`");
    }
    parse_next_token(parser);

    Statement *stmt = stmt_return(exp);
    stmt->loc = loc;
    return stmt;
}

//
// Parse a function definition.
//
static AstNode *parse_function(Parser *parser)
{
    //
    // <function> := "int" <identifier> "(" "void" ")" "{" <statement> "}"
    //
    AstNode *node = ast_function();
    node->loc = parser->tok.loc;

    AstFunction *func = &node->func;

    if (parser->tok.type != TOK_INT) {
        report_expected_err(&parser->tok, "`int`");        
    }

    parse_next_token(parser);
    if (parser->tok.type != TOK_ID) {
        report_expected_err(&parser->tok, "identifier");        
        func->name = safe_strdup("<unknown>");
    } else {
        func->name = safe_strdup(parser->tok.id);
    }

    parse_next_token(parser);
    if (parser->tok.type != '(') {
        report_expected_err(&parser->tok, "`(`");        
    }

    parse_next_token(parser);
    if (parser->tok.type != TOK_VOID) {
        report_expected_err(&parser->tok, "`void`");        
    }

    parse_next_token(parser);
    if (parser->tok.type != ')') {
        report_expected_err(&parser->tok, "`)`");        
    }

    parse_next_token(parser);
    if (parser->tok.type != '{') {
        report_expected_err(&parser->tok, "`{`");        
    }

    parse_next_token(parser);
    func->stmt = parse_statement(parser);

    if (parser->tok.type != '}') {
        report_expected_err(&parser->tok, "`}`");        
    }

    parse_next_token(parser);

    return node;
}

//
// Top level entry to the parser. Initialize, parse, and return an
// AST tree.
//
AstNode *parser_parse(Lexer *lex)
{
    Parser parser;

    parser.lex = lex;

    lexer_token(lex, &parser.tok);

    //
    // <program> := <function>
    //
    AstNode *prog = ast_program();
    prog->loc = parser.tok.loc;

    prog->prog.func = parse_function(&parser);

    if (parser.tok.type != TOK_EOF) {
        report_expected_err(&parser.tok, "end of file");        
    }

    return prog;
}