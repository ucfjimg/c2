#include "parser.h"

#include "errors.h"
#include "safemem.h"

typedef struct {
    Lexer *lex;
    Token tok;
} Parser;

typedef struct {
    TokenType tok;          // token type
    BinaryOp op;            // matching operator
    int prec_level;         // precedence level
} BinOpPrecedence;

//
// C precedence table 
//
static BinOpPrecedence bin_op_prec[] = {
    // () call, [] subscript, ->, ., ++/-- postfix      // 15   left assoc

    // ++/-- prefix, +/-/!/~ unary, (type) cast, 
    //    * deref, & addrof, sizeof                     // 14   right assoc

    { TOK_MULTIPLY,     BOP_MULTIPLY,       50 },       // 13   left assoc
    { TOK_DIVIDE,       BOP_DIVIDE,         50 },
    { TOK_MODULO,       BOP_MODULO,         50 },

    { TOK_PLUS,         BOP_ADD,            45 },       // 12   left assoc
    { TOK_MINUS,        BOP_SUBTRACT,       45 },

    { TOK_LSHIFT,       BOP_LSHIFT,         40 },       // 11   left assoc
    { TOK_RSHIFT,       BOP_RSHIFT,         40 },

    { TOK_LESSTHAN,     BOP_LESSTHAN,       35 },       // 10   left assoc
    { TOK_GREATERTHAN,  BOP_GREATERTHAN,    35 },
    { TOK_LESSEQUAL,    BOP_LESSEQUAL,      35 },
    { TOK_GREATEREQUAL, BOP_GREATEREQUAL,   35 },

    { TOK_EQUALITY,     BOP_EQUALITY,       30 },       // 9    left assoc
    { TOK_NOTEQUAL,     BOP_NOTEQUAL,       30 },

    { TOK_BITAND,       BOP_BITAND,         25 },       // 8    left assoc
    { TOK_BITXOR,       BOP_BITXOR,         24 },       // 7    left assoc
    { TOK_BITOR,        BOP_BITOR,          23 },       // 6    left assoc

    { TOK_LOGAND,       BOP_LOGAND,         10 },       // 5    left assoc
    { TOK_LOGOR,        BOP_LOGOR,           5 },       // 4    left assoc
    // ?: ternary right assoc                           // 3    right assoc
    
    // = (assignment) and all compound assignments      // 2    right assoc
    
    // ,                                                // 1    left assoc
};
static int bin_op_prec_count = sizeof(bin_op_prec) / sizeof(bin_op_prec[0]);

static Expression *parse_expression(Parser *parser, int min_prec);

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
        case TOK_LOGNOT:        *uop = UOP_LOGNOT; break;

        default:
            return false;
    }

    parse_next_token(parser);
    return true;
}

//
// Parse a factor.
// <factor> := <int> | <unop> <exp> | "(" <exp> ")"
//
static Expression *parse_factor(Parser *parser)
{
    FileLine loc = parser->tok.loc;

    //
    // <factor> := <int>
    //
    if (parser->tok.type == TOK_INT_CONST) {
        Expression *exp = exp_int(parser->tok.intval);
        exp->loc = loc;
        parse_next_token(parser);
        return exp;
    }

    //
    // <factor> := <unop> <exp> 
    //
    UnaryOp uop;
    if (parse_unary_op(parser, &uop)) {
        Expression *rhs = parse_factor(parser);
        Expression *exp = exp_unary(uop, rhs);
        exp->loc = loc;
        return exp;
    }

    //
    // <factor> := "(" <exp> ")"
    //
    if (parser->tok.type == '(') {
        parse_next_token(parser);
        Expression *exp = parse_expression(parser, 0);
        exp->loc = loc;
        if (parser->tok.type == ')') {
            parse_next_token(parser);
        } else {
            report_expected_err(&parser->tok, ")");
        }
        return exp;
    }

    //
    // Otherwise, parser error.
    //
    report_expected_err(&parser->tok, "constant, unary operator, or (");

    Expression *exp = exp_int(0);
    exp->loc = loc;
    return exp;
}

//
// Look up the given token in the precendence table. If found,
// return the precedence table entry, else NULL.
//
static BinOpPrecedence *get_binop_prec(TokenType token)
{
    for (int i = 0; i < bin_op_prec_count; i++) {
        if (bin_op_prec[i].tok == token) {
            return &bin_op_prec[i];
        }
    }
    return NULL;
}

//
// Parse an expression.
// <exp> := <factor> | <exp> <binop> <exp>
//
// Parses all binary operators using precedence climbing.
// `min_prec` is the minimum precedence level of operators to accept; if an
// operator is found that is lower precendence, then parsing will stop with
// that operator as the next token.
//
// Top level callers should set `min_prec` to zero.
//
static Expression *parse_expression(Parser *parser, int min_prec)
{
    Expression *left = parse_factor(parser);

    BinOpPrecedence *binop; 
    while ((binop = get_binop_prec(parser->tok.type)) != NULL) {
        if (binop->prec_level < min_prec) {
            break;
        }

        FileLine loc = parser->tok.loc;

        parse_next_token(parser);
        Expression *right = parse_expression(parser, binop->prec_level + 1);

        left = exp_binary(binop->op, left, right);
        left->loc = loc;
    }
    return left;
}

//
// Parse a statement.
// <statement> := "return" <exp> ";"
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
    Expression *exp = parse_expression(parser, 0);
    
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
// <function> := "int" <identifier> "(" "void" ")" "{" <statement> "}"
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