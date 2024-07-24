#include "parser.h"

#include "errors.h"
#include "safemem.h"

typedef struct {
    Lexer *lex;                 // the lexer
    Token tok;                  // the current token
} Parser;

typedef struct {
    Parser *parser;             // the owning parser
    LexerBookmark *lexer_bmrk;  // the corresponding lexer state
    Token tok;                  // the current token
} ParserBookmark;

typedef enum {
    AS_LEFT,
    AS_RIGHT
} Assoc;

typedef struct {
    TokenType tok;              // token type
    BinaryOp op;                // matching operator
    int prec_level;             // precedence level
    Assoc assoc;                // associativity
} BinOpPrecedence;

//
// C precedence table 
//
static BinOpPrecedence bin_op_prec[] = {
    // () call, [] subscript, ->, ., ++/-- postfix                           // 15   left assoc

    // ++/-- prefix, +/-/!/~ unary, (type) cast, 
    //    * deref, & addrof, sizeof                                          // 14   right assoc

    { TOK_MULTIPLY,             BOP_MULTIPLY,           50, AS_LEFT },       // 13   left assoc
    { TOK_DIVIDE,               BOP_DIVIDE,             50, AS_LEFT },
    { TOK_MODULO,               BOP_MODULO,             50, AS_LEFT },

    { TOK_PLUS,                 BOP_ADD,                45, AS_LEFT },       // 12   left assoc
    { TOK_MINUS,                BOP_SUBTRACT,           45, AS_LEFT },

    { TOK_LSHIFT,               BOP_LSHIFT,             40, AS_LEFT },       // 11   left assoc
    { TOK_RSHIFT,               BOP_RSHIFT,             40, AS_LEFT },

    { TOK_LESSTHAN,             BOP_LESSTHAN,           35, AS_LEFT },       // 10   left assoc
    { TOK_GREATERTHAN,          BOP_GREATERTHAN,        35, AS_LEFT },
    { TOK_LESSEQUAL,            BOP_LESSEQUAL,          35, AS_LEFT },
    { TOK_GREATEREQUAL,         BOP_GREATEREQUAL,       35, AS_LEFT },

    { TOK_EQUALITY,             BOP_EQUALITY,           30, AS_LEFT },       // 9    left assoc
    { TOK_NOTEQUAL,             BOP_NOTEQUAL,           30, AS_LEFT },

    { TOK_BITAND,               BOP_BITAND,             25, AS_LEFT },       // 8    left assoc
    { TOK_BITXOR,               BOP_BITXOR,             24, AS_LEFT },       // 7    left assoc
    { TOK_BITOR,                BOP_BITOR,              23, AS_LEFT },       // 6    left assoc

    { TOK_LOGAND,               BOP_LOGAND,             10, AS_LEFT },       // 5    left assoc
    { TOK_LOGOR,                BOP_LOGOR,              5,  AS_LEFT },       // 4    left assoc
    
    { TOK_QUESTION,             BOP_CONDITIONAL,        3,  AS_RIGHT },      // 3    right assoc
    
    { TOK_ASSIGN,               BOP_ASSIGN,             2,  AS_RIGHT },      // 2    right assoc
    { TOK_COMPOUND_ADD,         BOP_COMPOUND_ADD,       2,  AS_RIGHT },
    { TOK_COMPOUND_SUBTRACT,    BOP_COMPOUND_SUBTRACT,  2,  AS_RIGHT },
    { TOK_COMPOUND_MULTIPLY,    BOP_COMPOUND_MULTIPLY,  2,  AS_RIGHT },
    { TOK_COMPOUND_DIVIDE,      BOP_COMPOUND_DIVIDE,    2,  AS_RIGHT },
    { TOK_COMPOUND_MODULO,      BOP_COMPOUND_MODULO,    2,  AS_RIGHT },
    { TOK_COMPOUND_BITAND,      BOP_COMPOUND_BITAND,    2,  AS_RIGHT },
    { TOK_COMPOUND_BITOR,       BOP_COMPOUND_BITOR,     2,  AS_RIGHT },
    { TOK_COMPOUND_BITXOR,      BOP_COMPOUND_BITXOR,    2,  AS_RIGHT },
    { TOK_COMPOUND_LSHIFT,      BOP_COMPOUND_LSHIFT,    2,  AS_RIGHT },
    { TOK_COMPOUND_RSHIFT,      BOP_COMPOUND_RSHIFT,    2,  AS_RIGHT },

    // ,                                                                     // 1    left assoc
};
static int bin_op_prec_count = sizeof(bin_op_prec) / sizeof(bin_op_prec[0]);

static Expression *parse_expression(Parser *parser, int min_prec);
static Statement *parse_statement(Parser *parser);

//
// Create a parser bookmark at the current state.
//
static ParserBookmark *parse_bookmark(Parser *parser)
{
    ParserBookmark *bmrk = safe_zalloc(sizeof(ParserBookmark));

    bmrk->parser = parser;
    bmrk->lexer_bmrk = lexer_bookmark(parser->lex);
    token_clone(&parser->tok, &bmrk->tok);

    return bmrk;
}

//
// Jump to the given parser bookmark.
//
static void parse_goto_bookmark(ParserBookmark *bmrk)
{
    lexer_goto_bookmark(bmrk->lexer_bmrk);
    token_free(&bmrk->parser->tok);
    token_clone(&bmrk->tok, &bmrk->parser->tok);
}

//
// Free a parser bookmark.
//
static void parse_free_bookmark(ParserBookmark *bmrk)
{
    if (bmrk) {
        lexer_free_bookmark(bmrk->lexer_bmrk);
        token_free(&bmrk->tok);
        safe_free(bmrk);
    }
}

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
// Parse a unary operator, for prefix operators.
//
static bool parse_unary_op(Parser *parser, UnaryOp *uop)
{
    switch (parser->tok.type) {
        case TOK_PLUS:          *uop = UOP_PLUS; break;
        case TOK_MINUS:         *uop = UOP_MINUS; break;
        case TOK_COMPLEMENT:    *uop = UOP_COMPLEMENT; break;
        case TOK_LOGNOT:        *uop = UOP_LOGNOT; break;
        case TOK_INCREMENT:     *uop = UOP_PREINCREMENT; break;
        case TOK_DECREMENT:     *uop = UOP_PREDECREMENT; break;

        default:
            return false;
    }

    parse_next_token(parser);
    return true;
}

//
// Parse a primary expression.
// <primary> := <int> | <identifier> | "(" <exp> ")"
//
static Expression *parse_primary(Parser *parser)
{
    FileLine loc = parser->tok.loc;

    //
    // <primary> := <int>
    //
    if (parser->tok.type == TOK_INT_CONST) {
        Expression *exp = exp_int(parser->tok.intval, loc);
        parse_next_token(parser);
        return exp;
    }

    //
    // <primary> := <identifier>
    //
    if (parser->tok.type == TOK_ID) {
        Expression *exp = exp_var(parser->tok.id, loc);
        parse_next_token(parser);
        return exp;
    }

    //
    // <primary> := "(" <exp> ")"
    //
    if (parser->tok.type == '(') {
        parse_next_token(parser);
        Expression *exp = parse_expression(parser, 0);
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
    parse_next_token(parser);

    Expression *exp = exp_int(0, loc);
    return exp;
}

//
// Parse a postfix operator.
// <postfix> := <primary> | <postfix> <oper>  
//
static Expression *parse_postfix(Parser *parser)
{
    Expression *exp = parse_primary(parser);

    while (parser->tok.type == TOK_INCREMENT || parser->tok.type == TOK_DECREMENT) {
        if (parser->tok.type == TOK_INCREMENT) {
            exp = exp_unary(UOP_POSTINCREMENT, exp, parser->tok.loc);
        } else {
            exp = exp_unary(UOP_POSTDECREMENT, exp, parser->tok.loc);
        }

        parse_next_token(parser);
    }

    return exp;
}

//
// Parse a factor.
// <factor> := <unop> <factor> | <postfix>
//
static Expression *parse_factor(Parser *parser)
{
    FileLine loc = parser->tok.loc;

    //
    // <factor> := <unop> <factor> 
    //
    UnaryOp uop;
    if (parse_unary_op(parser, &uop)) {
        Expression *rhs = parse_factor(parser);
        Expression *exp = exp_unary(uop, rhs, loc);
        return exp;
    }

    return parse_postfix(parser);
}

//
// Parse the trueval part of a conditional operator cond ? trueval : falseval.
// The condition and '?' token have already been consumed.
//
static Expression *parse_conditional_trueval(Parser *parser)
{
    Expression *trueval = parse_expression(parser, 0);

    if (parser->tok.type != ':') {
        report_expected_err(&parser->tok, "`:`");
    } else {
        parse_next_token(parser);
    }

    return trueval;
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

        if (binop->op == BOP_CONDITIONAL) {
            //
            // Special handling for the conditional operator a ? b : c
            //
            Expression *trueval = parse_conditional_trueval(parser);
            Expression *falseval = parse_expression(parser, binop->prec_level);
            left = exp_conditional(left, trueval, falseval, loc);
        } else if (binop->assoc == AS_RIGHT) {
            //
            // right associative
            //
            Expression *right = parse_expression(parser, binop->prec_level);
            left = exp_assignment(binop->op, left, right, loc);
        } else {
            Expression *right = parse_expression(parser, binop->prec_level + 1);
            left = exp_binary(binop->op, left, right, loc);
        }
    }
    return left;
}

//
// Parse a declaration.
// <declaration> := "int" <identifier> [ "=" <exp> ] ";" 
// The type has already been parsed.
//
static Declaration *parse_declaration(Parser *parser)
{
    Declaration *decl = NULL;
    char *name = NULL;
    Expression *init = NULL;
    FileLine loc = parser->tok.loc;

    if (parser->tok.type != TOK_ID) {
        report_expected_err(&parser->tok, "identifier");
        goto done;
    }
    name = safe_strdup(parser->tok.id);
    parse_next_token(parser);

    if (parser->tok.type == TOK_ASSIGN) {
        parse_next_token(parser);
        init = parse_expression(parser, 0);
    }

    if (parser->tok.type != ';') {
        report_expected_err(&parser->tok, "`;`");
    } else {
        parse_next_token(parser);
    }

    decl = declaration(name, init, loc);
    init = NULL;

done:
    if (decl == NULL) {
        if (name == NULL) {
            //
            // If there's no name, just add a dummy declaration.
            //
            decl = declaration(".error", NULL, loc);
        } else {
            decl = declaration(name, NULL, loc);
        }
    }
    exp_free(init);
    safe_free(name);
    return decl;
}

//
// Parse a block until a '}'. Expects a leading '{'; does not consume
// the trailing '}'. 
//
// Returns a list of BlockItem.
//
static List parse_block(Parser *parser)
{
    List items;
    list_clear(&items);

    if (parser->tok.type != '}') {
        while (parser->tok.type != '}' && parser->tok.type != TOK_EOF) {
            BlockItem *blki = NULL;
            //
            // In the future, this will be on any type, not just `int`.
            //
            if (parser->tok.type == TOK_INT) {
                parse_next_token(parser);
                Declaration *decl = parse_declaration(parser);
                blki = blki_declaration(decl);
            } else {
                Statement *stmt = parse_statement(parser);
                blki = blki_statement(stmt);
            }

            list_push_back(&items, &blki->list);
        }
    }

    if (parser->tok.type != '}') {
        report_expected_err(&parser->tok, "`}`");        
    } 

    return items;
}

//
// Parse a compound statement.
// <statement> := "{" { <block-item> } "}"
// The leading "{" has already been consumed.
//
static Statement *parse_compound_statement(Parser *parser)
{
    List items = parse_block(parser);
    FileLine loc = parser->tok.loc;

    if (parser->tok.type != '}') {
        report_expected_err(&parser->tok, "`}`");
    } else {
        parse_next_token(parser);
    }

    return stmt_compound(items, loc);
}

//
// Parse a return statement. 
// The `return` keyword token has already been consumed.
//
// <statement> := "return" [ <expression> ] ";"
//
static Statement *parse_stmt_return(Parser *parser)
{
    FileLine loc = parser->tok.loc;
    Expression *retval = NULL;

    Expression *exp = parse_expression(parser, 0);
    
    if (parser->tok.type != ';') {
        report_expected_err(&parser->tok, "`;`");
    } else {
        parse_next_token(parser);
    }

    Statement *stmt = stmt_return(exp, loc);
    exp = NULL;

    exp_free(retval);
    return stmt;
}

//
// Parse an if statement. 
// The `if` keyword token has already been consumed.
//
// <statement> :=  "if" "(" <exp> ")" <statement> [ "else" <statement> ]
//
static Statement *parse_stmt_if(Parser *parser)
{
    FileLine loc = parser->tok.loc;
    Expression *condition = NULL;
    Statement *thenpart = NULL;
    Statement *elsepart = NULL;

    if (parser->tok.type == '(') {
        parse_next_token(parser);
    } else {
        report_expected_err(&parser->tok, "`(`");
    }

    condition = parse_expression(parser, 0);

    if (parser->tok.type == ')') {
        parse_next_token(parser);
    } else {
        report_expected_err(&parser->tok, "`)`");
    }

    thenpart = parse_statement(parser);

    if (parser->tok.type == TOK_ELSE) {
        parse_next_token(parser);
        elsepart = parse_statement(parser);
    }    

    Statement *stmt = stmt_if(condition, thenpart, elsepart, loc);

    return stmt;
}

//
// Parse a label statement.
// <statement> := <identifier> ":" <statement>
//
// The identifier and colon have already been consumed.
//
static Statement *parse_label(Parser *parser, char *label, FileLine loc)
{
    //
    // A label must be followed by another statement.
    //
    Statement *labeled_stmt = parse_statement(parser);

    return stmt_label(label, labeled_stmt, loc);
}

//
// Parse a goto statement.
// <statement> := "goto" <identifier> ";"
//
// The goto keyword has already been consumed.
//
static Statement *parse_goto(Parser *parser)
{
    char *label = NULL;
    FileLine loc = parser->tok.loc;

    if (parser->tok.type != TOK_ID) {
        report_expected_err(&parser->tok, "label");
    } else {
        label = safe_strdup(parser->tok.id);
        parse_next_token(parser);
    }

    if (parser->tok.type != ';') {
        report_expected_err(&parser->tok, "`;`");
    } else {
        parse_next_token(parser);
    }

    Statement *stmt = NULL;
    if (label) {
        stmt = stmt_goto(label, loc);
        safe_free(label);
    } else {
        stmt = stmt_null(loc);
    }

    return stmt;
}

//
// Parse a break statement.
// <statement> := "break" ";"
// the break keyword has already been consumed.
// 
static Statement *parse_break(Parser *parser, FileLine loc)
{
    if (parser->tok.type != ';') {
        report_expected_err(&parser->tok, "`;`");
    } else {
        parse_next_token(parser);
    }

    return stmt_break(loc);
}

//
// Parse a continue statement.
// <statement> := "continue" ";"
// the continue keyword has already been consumed.
// 
static Statement *parse_continue(Parser *parser, FileLine loc)
{
    if (parser->tok.type != ';') {
        report_expected_err(&parser->tok, "`;`");
    } else {
        parse_next_token(parser);
    }

    return stmt_continue(loc);
}

//
// Parse a while loop.
// <statment> := "while" "(" <exp> ")" <statement>
// the while keyword has already been consumed.
//
static Statement *parse_while(Parser *parser, FileLine loc)
{
    Expression *cond = NULL;
    Statement *body = NULL;

    if (parser->tok.type != '(') {
        report_expected_err(&parser->tok, "`(`");
    } else {
        parse_next_token(parser);
    }

    cond = parse_expression(parser, 0);

    if (parser->tok.type != ')') {
        report_expected_err(&parser->tok, "`)`");
    } else {
        parse_next_token(parser);
    }

    body = parse_statement(parser);

    return stmt_while(cond, body, loc);
}

//
// Parse a do while loop.
// <statment> := "do" <statement> "while" "(" <exp> ")" ";"
// the do keyword has already been consumed.
//
static Statement *parse_do_while(Parser *parser, FileLine loc)
{
    Expression *cond = NULL;
    Statement *body = NULL;

    body = parse_statement(parser);

    if (parser->tok.type != TOK_WHILE) {
        report_expected_err(&parser->tok, "`while`");
    } else {
        parse_next_token(parser);
    }

    if (parser->tok.type != '(') {
        report_expected_err(&parser->tok, "`(`");
    } else {
        parse_next_token(parser);
    }

    cond = parse_expression(parser, 0);

    if (parser->tok.type != ')') {
        report_expected_err(&parser->tok, "`)`");
    } else {
        parse_next_token(parser);
    }

    if (parser->tok.type != ';') {
        report_expected_err(&parser->tok, "`;`");
    } else {
        parse_next_token(parser);
    }

    return stmt_do_while(cond, body, loc);
}

//
// Parse a for initializer.
// <forinit> := ';' | <declaration> ';' | <exp> ';'
//
static ForInit *parse_forinit(Parser *parser)
{
    if (parser->tok.type == ';') {
        parse_next_token(parser);
        return forinit();
    }

    //
    // TODO when there are real types, this will check for a type,
    // not just int.
    //
    if (parser->tok.type == TOK_INT) {
        parse_next_token(parser);
        Declaration *decl = parse_declaration(parser);
        
        //
        // Note that the definition for declaration includes the ';'.
        //
        
        return forinit_decl(decl);
    }

    Expression *exp = parse_expression(parser, 0);

    if (parser->tok.type == ';') {
        parse_next_token(parser);
    } else {
        report_expected_err(&parser->tok, "`;`");
    }

    return forinit_exp(exp);
}

//
// Parse a for statement.
// <statement> := "for" "(" <for-init> ";" <exp> ";" <exp> ")" <statement>
// The for keyword has already been consumed.
//
static Statement *parse_for(Parser *parser, FileLine loc)
{
    if (parser->tok.type != '(') {
        report_expected_err(&parser->tok, "`(`");
    } else {
        parse_next_token(parser);
    }

    //
    // for initializer
    // <forinit> := ';' | <declaration> ';' | <exp> ';'
    // Note that this includings the trailing ';'
    //
    ForInit *fi = parse_forinit(parser);

    //
    // optional condition
    //
    Expression *cond = NULL;
    if (parser->tok.type != ';') {
        cond = parse_expression(parser, 0);
    }

    if (parser->tok.type == ';') {
        parse_next_token(parser);
    } else {
        report_expected_err(&parser->tok, "`;`");
    }

    //
    // optional post expression
    //
    Expression *post = NULL;
    if (parser->tok.type != ')') {
        post = parse_expression(parser, 0);
    }

    if (parser->tok.type == ')') {
        parse_next_token(parser);
    } else {
        report_expected_err(&parser->tok, "`)`");
    }

    //
    // Statement
    //
    Statement *body = parse_statement(parser);

    return stmt_for(fi, cond, post, body, loc);
}

//
// Parse a statement.
// <statement> := 
//      "{" { <block-item> } "}"
//      ";" | 
//      "return" <exp> ";" | 
//      "if" "(" <exp> ")" <statement> [ "else" <statement> ] | 
//      <identifier> ":" <statement> |
//      "goto" <identifier> ";" |
//      <exp> ";" |
//      "break" ";" |
//      "continue ";" |
//      "while" "(" <exp> ")" <statement> |
//      "do" <statment> "while" "(" <exp> ")" ";" |
//      "for" "(" <for-init> ";" <exp> ";" <exp> ")" <statement>
//
static Statement *parse_statement(Parser *parser)
{
    FileLine loc = parser->tok.loc;

    //
    // <statement> := "{" { <block-item> } "}"
    //
    if (parser->tok.type == '{') {
        parse_next_token(parser);
        return parse_compound_statement(parser);
    }

    //
    // <statement> := ";"
    //
    if (parser->tok.type == ';') {
        parse_next_token(parser);
        return stmt_null(loc);
    }

    //
    // <statement> := "return" <exp> ";" 
    //
    if (parser->tok.type == TOK_RETURN) {
        parse_next_token(parser);
        return parse_stmt_return(parser);
    }

    //
    // <statement> := "if" "(" <exp> ")" <statement> [ "else" <statement> ]
    //
    if (parser->tok.type == TOK_IF) {
        parse_next_token(parser);
        return parse_stmt_if(parser);
    }

    //
    // <statement> := <identifier> ":" <statement>
    //
    // We have to look ahead a bit here -- <identifier>, if followed by ":", is a
    // label; but otherwise it's the start of an expression.
    //
    if (parser->tok.type == TOK_ID) {
        ParserBookmark *bmrk = parse_bookmark(parser);
        parse_next_token(parser);
        if (parser->tok.type == ':') {            
            parse_next_token(parser);
            Statement *label = parse_label(parser, bmrk->tok.id, bmrk->tok.loc);
            parse_free_bookmark(bmrk);
            return label;
        } 

        parse_goto_bookmark(bmrk);
        parse_free_bookmark(bmrk);
    }

    //
    // <statement> := "goto" <identifier> ";"
    //
    if (parser->tok.type == TOK_GOTO) {
        parse_next_token(parser);
        return parse_goto(parser);
    }

    //
    // <statement> := "break" ";"
    //
    if (parser->tok.type == TOK_BREAK) {
        parse_next_token(parser);
        return parse_break(parser, loc);
    }

    //
    // <statement> := "continue" ";"
    //
    if (parser->tok.type == TOK_CONTINUE) {
        parse_next_token(parser);
        return parse_continue(parser, loc);
    }

    //
    // <statement> := "while" "(" <exp> ")" <statement>
    //
    if (parser->tok.type == TOK_WHILE) {
        parse_next_token(parser);
        return parse_while(parser, loc);
    }

    //
    // <statement> := "do" <statment> "while" "(" <exp> ")" ";"
    //
    if (parser->tok.type == TOK_DO) {
        parse_next_token(parser);
        return parse_do_while(parser, loc);
    }

    //
    // <statement> := "for" "(" <for-init> ";" <exp> ";" <exp> ")" <statement>
    //
    if (parser->tok.type == TOK_FOR) {
        parse_next_token(parser);
        return parse_for(parser, loc);
    }

    //
    // <statement> = <exp> ";" 
    //
    Expression *exp = parse_expression(parser, 0);
    if (parser->tok.type != ';') {
        report_expected_err(&parser->tok, "`;`");
    } else {
        parse_next_token(parser);
    }
    return stmt_expression(exp, loc);
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
    char *name = NULL;
    List stmts;

    if (parser->tok.type != TOK_INT) {
        report_expected_err(&parser->tok, "`int`");        
    }

    parse_next_token(parser);
    if (parser->tok.type != TOK_ID) {
        report_expected_err(&parser->tok, "identifier");        
        name = safe_strdup("<unknown>");
    } else {
        name = safe_strdup(parser->tok.id);
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
    stmts = parse_block(parser);

    if (parser->tok.type != '}') {
        report_expected_err(&parser->tok, "`}`");        
    }

    parse_next_token(parser);

    AstNode *node = ast_function(name, stmts, parser->tok.loc);

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
    AstNode *prog = ast_program(parser.tok.loc);

    prog->prog.func = parse_function(&parser);

    if (parser.tok.type != TOK_EOF) {
        report_expected_err(&parser.tok, "end of file");        
    }

    return prog;
}