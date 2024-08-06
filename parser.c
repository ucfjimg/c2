#include "parser.h"

#include "errors.h"
#include "ice.h"
#include "safemem.h"

#include <stdbool.h>

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
static List parse_block(Parser *parser);
static TypeSpecifier *parse_type_specifier(Parser *parser);
static bool is_type_specifier(Parser *parser);

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
// <primary> := <int> | <long> | <uint> | <ulong> | <float> | <identifier> | "(" <exp> ")"
//
static Expression *parse_primary(Parser *parser)
{
    FileLine loc = parser->tok.loc;

    //
    // <primary> := <int> | <long> | <uint> | <ulong>
    //
    if (parser->tok.type == TOK_INT_CONST) {
        Expression *exp;
        
        if (parser->tok.int_const.is_long && parser->tok.int_const.is_unsigned) {
            exp = exp_ulong(parser->tok.int_const.intval, loc);
        } else if (parser->tok.int_const.is_long) {
            exp = exp_long(parser->tok.int_const.intval, loc);
        } else if (parser->tok.int_const.is_unsigned) {
            exp = exp_uint(parser->tok.int_const.intval, loc);
        } else {
            exp = exp_int(parser->tok.int_const.intval, loc);
        }
        parse_next_token(parser);
        return exp;
    }
    
    //
    // <primary> := <float>
    //
    if (parser->tok.type == TOK_FLOAT_CONST) {
        Expression *exp = exp_float(parser->tok.float_const, loc);
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
    // <primary> := "(" <exp> ")" | "(" { <specifier> }+ ")"
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
// Parse a function call operator.
// <postfix> := "(" <arg-list> ")"
//
static Expression *parse_function_call(Parser *parser, Expression *func)
{
    FileLine loc = parser->tok.loc;

    ICE_ASSERT(parser->tok.type == '(');
    parse_next_token(parser);

    List args;
    list_clear(&args);
    
    if (parser->tok.type != ')') {
        while (true) {
            Expression *parm = parse_expression(parser, 0);
            list_push_back(&args, &parm->list);

            if (parser->tok.type == ',') {
                parse_next_token(parser);
                continue;
            } 

            if (parser->tok.type != ')') {
                report_expected_err(&parser->tok, "`)` or `,`");
            } else {
                parse_next_token(parser);
            }

            break;
        }
    } else {
        //
        // Consume ')' of empty parameter list.
        //
        parse_next_token(parser);
    }

    //
    // TODO for now, without types, a function must just be an identifier of the
    // function name.
    //
    char *name = NULL;
    if (func->tag != EXP_VAR) {
        err_report(EC_ERROR, &loc, "function pointers are not supported.");
        name = safe_strdup(".error");
    } else {
        name = safe_strdup(func->var.name);
    }

    Expression *exp = exp_function_call(name, args, loc);

    //
    // We only needed the name from the function expression, free it since
    // we will not hold a reference to it.
    //
    safe_free(name);
    exp_free(func);
    return exp;
} 

//
// Parse a postfix operator.
// <postfix> := <primary> | <postfix> "++" | <postfix> "--" | <postfix> "(" <arg-list> ")" 
//
static Expression *parse_postfix(Parser *parser)
{
    Expression *exp = parse_primary(parser);

    while (true) {
        switch (parser->tok.type) {
            case TOK_INCREMENT: 
                exp = exp_unary(UOP_POSTINCREMENT, exp, parser->tok.loc); 
                parse_next_token(parser);
                break;

            case TOK_DECREMENT: 
                exp = exp_unary(UOP_POSTDECREMENT, exp, parser->tok.loc); 
                parse_next_token(parser);
                break;
            
            case '(':           exp = parse_function_call(parser, exp); break;
            default: goto done;
        }
    }

done:
    return exp;
}

//
// Parse a factor.
// <factor> := <unop> <factor> | "(" { <specifier> }+ ")" <fcator> | <postfix>
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


    //
    // '(' could be the start of a cast, or at a higher precedence,
    // a function call or precedence operator. Check for a type to
    // see if it's a cast.
    //
    // <factor> := "(" { <specifier> }+ ")" <factor>
    //
    if (parser->tok.type == '(') {
        ParserBookmark *bmrk = parse_bookmark(parser);
        parse_next_token(parser);

        if (is_type_specifier(parser)) {
            parse_free_bookmark(bmrk);
            TypeSpecifier *ts = parse_type_specifier(parser);

            if (ts->sc != SC_NONE) {
                err_report(EC_ERROR, &parser->tok.loc, "cast operator may not include storage class.");
            }

            if (parser->tok.type != ')') {
                report_expected_err(&parser->tok, "`)`");
            } else {
                parse_next_token(parser);
            }

            Expression *exp = parse_factor(parser);
            Expression *cast = exp_cast(typespec_take_type(ts), exp, parser->tok.loc);

            typespec_free(ts);
            return cast;
        }

        parse_goto_bookmark(bmrk);
        parse_free_bookmark(bmrk);
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
// Return true if the current token is a type specifier.
//
static bool is_type_specifier(Parser *parser)
{
    TokenType tt = parser->tok.type;

    return
        tt == TOK_INT ||
        tt == TOK_LONG ||
        tt == TOK_SIGNED ||
        tt == TOK_UNSIGNED ||
        tt == TOK_DOUBLE ||
        tt == TOK_EXTERN ||
        tt == TOK_STATIC;
}

//
// Parse the type specifiers of a declaration:
//
// <specifiers> := { "int" | "long" | "signed" | "unsigned" | "double" |"extern" | "static" }
//
static TypeSpecifier *parse_type_specifier(Parser *parser)
{
    enum {
        TOK_INT_INDEX,
        TOK_LONG_INDEX,
        TOK_SIGNED_INDEX,
        TOK_UNSIGNED_INDEX,
        TOK_DOUBLE_INDEX,
        TOK_EXTERN_INDEX,
        TOK_STATIC_INDEX
    };

    struct {
        TokenType tt;
        int count;
    } spec_tokens[] = {
        { TOK_INT, 0 },
        { TOK_LONG, 0 },
        { TOK_SIGNED, 0 },
        { TOK_UNSIGNED, 0 },
        { TOK_DOUBLE, 0 },
        { TOK_EXTERN, 0 },
        { TOK_STATIC, 0 },
    };
    const int spec_token_count = sizeof(spec_tokens) / sizeof(spec_tokens[0]);

    FileLine loc = parser->tok.loc;

    //
    // Consume and count all tokens that count as a type specifier.
    //
    bool was_spec_token;
    do {
        was_spec_token = false;  
        for (int i = 0; i < spec_token_count && !was_spec_token; i++) {
            if (parser->tok.type == spec_tokens[i].tt) {
                spec_tokens[i].count++;
                was_spec_token = true;
                parse_next_token(parser);
            }
        }
    } while (was_spec_token);

    //
    // Validate that the tokens are syntactically correct.
    //
    for (int i = 0; i < spec_token_count; i++) {
        if (spec_tokens[i].count > 1) {
            char *err_tok = token_type_describe(spec_tokens[i].tt);
            err_report(EC_ERROR, &loc, "type specifier `%s` may only be given once.", err_tok);
            safe_free(err_tok);
        }
    }

    if (spec_tokens[TOK_EXTERN_INDEX].count && spec_tokens[TOK_STATIC_INDEX].count) {
        err_report(EC_ERROR, &loc, "`static` and `extern` are mutually exclusive.");
    }

    if (spec_tokens[TOK_SIGNED_INDEX].count && spec_tokens[TOK_UNSIGNED_INDEX].count) {
        err_report(EC_ERROR, &loc, "`signed` and `unsigned` are mutually exclusive.");
    }

    bool has_int = 
        spec_tokens[TOK_INT_INDEX].count || 
        spec_tokens[TOK_LONG_INDEX].count ||
        spec_tokens[TOK_SIGNED_INDEX].count ||
        spec_tokens[TOK_UNSIGNED_INDEX].count;

    bool has_double = spec_tokens[TOK_DOUBLE_INDEX].count;

    if (!(has_int || spec_tokens[TOK_DOUBLE_INDEX].count)) {
        err_report(EC_ERROR, &loc, "missing type specifier.");
    }

    if (has_double && has_int) {
        err_report(EC_ERROR, &loc, "`double` may not be combined with integral type specifiers.");
    }

    StorageClass sc = SC_NONE;
    if (spec_tokens[TOK_STATIC_INDEX].count) {
        sc = SC_STATIC;
    } else if (spec_tokens[TOK_EXTERN_INDEX].count) {
        sc = SC_EXTERN;
    }

    if (has_double) {
        return typespec_alloc(sc, type_double());
    }

    bool is_unsigned = spec_tokens[TOK_UNSIGNED_INDEX].count;
    bool is_long = spec_tokens[TOK_LONG_INDEX].count;

    if (is_unsigned && is_long) {
        return typespec_alloc(sc, type_ulong());
    } else if (is_unsigned) {
        return typespec_alloc(sc, type_uint());
    } else if (is_long) {
        return typespec_alloc(sc, type_long());
    }

    return typespec_alloc(sc, type_int());
}

//
// Parse a variable declaration.
// <declaration> := { <specifier> }+ <identifier> [ "=" <exp> ] ";"  |
//
// The type and identifier have already been parsed.
//
static Declaration *parse_decl_variable(Parser *parser, TypeSpecifier *typespec, char *name, FileLine loc)
{
    Expression *init = NULL;

    if (parser->tok.type == TOK_ASSIGN) {
        parse_next_token(parser);
        init = parse_expression(parser, 0);
    }

    if (parser->tok.type != ';') {
        report_expected_err(&parser->tok, "`;`");
    } else {
        parse_next_token(parser);
    }

    return decl_variable(name, typespec_take_type(typespec), typespec->sc, init, loc);
}

//
// Parse a function declaration.
// <declaration> := { <specifier> }+ <identifier> "(" <param-list> ")" ";"
//
// Tokens up to and including the opening '(' have been parsed.
//
// `typespec` is the parsed return type.
//
static Declaration *parse_decl_function(Parser *parser, TypeSpecifier *typespec, char *name, FileLine loc)
{
    List parms;
    List ptypes;
    bool parm_list_err = false;
    bool is_void = false;

    //
    // parms is the list of identifiers of the parameter list, which goes in the AST.
    // ptypes is the list of types in the parameter list, which goes in the function type.
    //
    list_clear(&parms);
    list_clear(&ptypes);

    //
    // Special case - "(" "void" ")" means empty parameter list.
    //
    if (parser->tok.type == TOK_VOID) {
        parse_next_token(parser);
        is_void = true;

        if (parser->tok.type != ')') {
            parm_list_err = true;
            report_expected_err(&parser->tok, "`)`");
        } else {
            parse_next_token(parser);
        }
    } else {
        while (true) {
            TypeSpecifier *ptypespec = parse_type_specifier(parser);
            
            if (ptypespec->sc != SC_NONE) {
                err_report(EC_ERROR, &parser->tok.loc, "function parameters may not have storage classes.");
                parm_list_err = true;
                break;
            }

            char *parm_name = NULL;
            if (parser->tok.type != TOK_ID) {
                parm_list_err = true;
                typespec_free(ptypespec);
                report_expected_err(&parser->tok, "identifier");
                break;
            } else {
                parm_name = safe_strdup(parser->tok.id);
                parse_next_token(parser);
            }

            FuncParameter *parm = func_parm(parm_name);
            list_push_back(&parms, &parm->list);

            TypeFuncParam *tparm = type_func_param(typespec_take_type(ptypespec));
            list_push_back(&ptypes, &tparm->list);

            typespec_free(ptypespec);
            safe_free(parm_name);

            if (parser->tok.type == ',') {
                parse_next_token(parser);
                continue;
            }

            break;
        }

        if (parser->tok.type != ')') {
            parm_list_err = true;
            report_expected_err(&parser->tok, "`,` or `)`");
        } else {
            parse_next_token(parser);
        }
    }

    Type *functype = type_function(typespec_take_type(typespec), ptypes);

    bool has_body = false;
    List body;
    list_clear(&body);
    if (parser->tok.type == '{') {
        parse_next_token(parser);
        has_body = true;

        body = parse_block(parser);

        if (parser->tok.type != '}') {
            report_expected_err(&parser->tok, "`}`");
        } else {
            parse_next_token(parser);
        }
    } else if (parser->tok.type == ';') {
        parse_next_token(parser);
    } else {
        report_expected_err(&parser->tok, "`{` or `;`");
    }

    if (!parm_list_err && !is_void && parms.head == NULL) {
        err_report(EC_ERROR, &loc, "`()` is not valid for a function with no parameters; use `(void)`.");
    }

    return decl_function(name, functype, typespec->sc, parms, body, has_body, loc);    
}

//
// Parse a declaration.
// <declaration> := 
//    { <specifier> }+ <identifier> [ "=" <exp> ] ";"  |
//    { <specifier> }+ <identifier> "(" <param-list> ")" ( <block> | ";" )
//
// The type has already been parsed.
//
static Declaration *parse_declaration(Parser *parser)
{
    Declaration *decl = NULL;
    char *name = NULL;
    FileLine loc = parser->tok.loc;
    TypeSpecifier *typespec = NULL;

    typespec = parse_type_specifier(parser);

    if (parser->tok.type != TOK_ID) {
        report_expected_err(&parser->tok, "identifier");
        parse_next_token(parser);
        goto done;
    }
    name = safe_strdup(parser->tok.id);
    parse_next_token(parser);

    if (parser->tok.type == '(') {
        parse_next_token(parser);
        decl = parse_decl_function(parser, typespec, name, loc);
    } else {
        decl = parse_decl_variable(parser, typespec, name, loc);
    }

done:
    if (decl == NULL) {
        //
        // If there's no name, just add a dummy declaration.
        //
        decl = decl_variable(".error", type_int(), SC_NONE, NULL, loc);
    }
    typespec_free(typespec);
    safe_free(name);
    return decl;
}

//
// Parse a block until a '}'. Expects a leading '{' to already be consumed; 
// does not consume the trailing '}'. 
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

            if (is_type_specifier(parser)) {
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

    if (is_type_specifier(parser)) {
        Declaration *decl = parse_declaration(parser);

        if (decl->tag == DECL_FUNCTION) {
            err_report(EC_ERROR, &decl->loc, "for loop initializer may not declare a function.\n");
        }
        
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
// Parse a switch statment.
// <statement> := "switch" "(" cond ")" <statement>
// The switch keyword has already been consumed.
//
static Statement *parse_switch(Parser *parser, FileLine loc)
{
    if (parser->tok.type != '(') {
        report_expected_err(&parser->tok, "`(`");
    } else {
        parse_next_token(parser);
    }

    Expression *cond = parse_expression(parser, 0);

    if (parser->tok.type != ')') {
        report_expected_err(&parser->tok, "`)`");
    } else {
        parse_next_token(parser);
    }

    Statement *body = parse_statement(parser);

    return stmt_switch(cond, body, loc);
}

//
// Parse a case statment.
// <statement> := "case" <const-int> ":" <statement>
// The case keyword has already been consumed.
//
static Statement *parse_case(Parser *parser, FileLine loc)
{
    int value = 0;

    if (parser->tok.type != TOK_INT_CONST) {
        report_expected_err(&parser->tok, "integer constant");
    } else {
        //
        // TODO when we have proper typing, this will be converted to
        // the proper size/signedness value.
        //
        value = (int)parser->tok.int_const.intval;
        parse_next_token(parser);
    }

    if (parser->tok.type != ':') {
        report_expected_err(&parser->tok, "`:`");
    } else {
        parse_next_token(parser);
    }

    Statement *stmt = parse_statement(parser);

    return stmt_case(value, stmt, loc);
}

//
// Parse a default statment.
// <statement> := "default" ":" <statement>
// The default keyword has already been consumed.
//
static Statement *parse_default(Parser *parser, FileLine loc)
{
    if (parser->tok.type != ':') {
        report_expected_err(&parser->tok, "`:`");
    } else {
        parse_next_token(parser);
    }

    Statement *stmt = parse_statement(parser);

    return stmt_default(stmt, loc);
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
//      "switch" "(" cond ")" <statement>
//      "case" <const-int> ":" <statement>
//      "default" ":" <statement> 
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
    // <statement> := "switch" "(" cond ")" <statement>
    //
    if (parser->tok.type == TOK_SWITCH) {
        parse_next_token(parser);
        return parse_switch(parser, loc);
    }
    
    //
    // <statement> := "case" <const-int> ":" <statement>
    //
    if (parser->tok.type == TOK_CASE) {
        parse_next_token(parser);
        return parse_case(parser, loc);
    }

    //
    // "default" ":" <statement> 
    //
    if (parser->tok.type == TOK_DEFAULT) {
        parse_next_token(parser);
        return parse_default(parser, loc);
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
// Top level entry to the parser. Initialize, parse, and return an
// AST tree.
//
AstProgram *parser_parse(Lexer *lex)
{
    Parser parser;

    parser.lex = lex;

    lexer_token(lex, &parser.tok);

    FileLine loc = parser.tok.loc;

    //
    // <program> := { <declaration> }
    //
    List decls;
    list_clear(&decls);

    while (parser.tok.type != TOK_EOF) {
        Declaration *decl = parse_declaration(&parser);        
        list_push_back(&decls, &decl->list);
    }

    return ast_program(decls, loc);   
}
