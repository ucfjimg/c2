#pragma once

#include "fileline.h"
#include "list.h"
#include "operators.h"

#include <stdbool.h>

typedef struct Expression Expression;
typedef struct Declaration Declaration;
typedef struct Statement Statement;
typedef struct BlockItem BlockItem;
typedef struct AstNode AstNode;

//
// expressions
//
typedef enum { 
    EXP_INT,
    EXP_VAR,
    EXP_UNARY,
    EXP_BINARY,
    EXP_CONDITIONAL,
    EXP_ASSIGNMENT,
} ExpressionTag;

typedef struct {
    char *name;
} ExpVar;

typedef struct {
    UnaryOp op;
    Expression *exp;
} ExpUnary;

typedef struct {
    BinaryOp op;
    Expression *left;
    Expression *right;
} ExpBinary;

typedef struct {
    Expression *cond;
    Expression *trueval;
    Expression *falseval;
} ExpConditional;

typedef struct {
    BinaryOp op;
    Expression *left;
    Expression *right;
} ExpAssignment;

struct Expression {
    ExpressionTag tag;
    FileLine loc;               

    union {
        unsigned long intval;
        ExpVar var;
        ExpUnary unary;
        ExpBinary binary;
        ExpConditional conditional;
        ExpAssignment assignment;
    };
};

extern Expression *exp_int(unsigned long intval, FileLine loc);
extern Expression *exp_var(char *name, FileLine loc);
extern Expression *exp_unary(UnaryOp op, Expression *exp, FileLine loc);
extern Expression *exp_binary(BinaryOp op, Expression *left, Expression *right, FileLine loc);
extern Expression *exp_conditional(Expression *conditional, Expression *trueval, Expression *falseval, FileLine loc);
extern Expression *exp_assignment(BinaryOp op, Expression *left, Expression *right, FileLine loc);
extern void exp_free(Expression *exp);

//
// declarations
//
struct Declaration {
    FileLine loc;
    char *name;                 // name of variable being declared
    Expression *init;           // initializatiom (may be NULL)
};

extern Declaration *declaration(char *name, Expression *init, FileLine loc);
extern void declaration_free(Declaration *decl);

//
// statemements
//
typedef enum {
    STMT_NULL,
    STMT_RETURN,
    STMT_IF,
    STMT_EXPRESSION,
    STMT_LABEL,
    STMT_GOTO,
    STMT_COMPOUND,
} StatementTag;

typedef struct {
    Expression *exp;            // return value (may be NULL) 
} StmtReturn;

typedef struct {
    Expression *condition;      // condition to check
    Statement *thenpart;        // `then` part statement (must exist)
    Statement *elsepart;        // `else` part statement (optional)
} StmtIf;

typedef struct {
    Expression *exp;            // expression used as statement
} StmtExpression;

typedef struct {
    char *name;                 // label name
    Statement *stmt;            // labeled statement
} StmtLabel;

typedef struct {
    char *target;               // target location
} StmtGoto;

typedef struct {
    List items;                 // of <BlockItem>
} StmtCompound;

struct Statement {
    ListNode list;
    StatementTag tag;
    FileLine loc;               

    union {
        StmtReturn ret;         // STMT_RETURN
        StmtIf ifelse;          // STMT_IF
        StmtExpression exp;     // STMT_EXPRESSION
        StmtLabel label;        // STMT_LABEL
        StmtGoto goto_;         // STMT_GOTO
        StmtCompound compound;  // STMT_COMPOUND
    };
};

extern Statement *stmt_null(FileLine loc);
extern Statement *stmt_return(Expression *exp, FileLine loc);
extern Statement *stmt_if(Expression *condition, Statement *thenpart, Statement *elsepart, FileLine loc);
extern Statement *stmt_expression(Expression *exp, FileLine loc);
extern Statement *stmt_label(char *name, Statement *stmt, FileLine loc);
extern Statement *stmt_goto(char *target, FileLine loc);
extern Statement *stmt_compound(List items, FileLine loc);
extern void stmt_free(Statement *stmt);

//
// A block item is either a declaration or a statement.
//
typedef enum {
    BI_DECLARATION,
    BI_STATEMENT,
} BlockItemTag;

struct BlockItem {
    ListNode list;
    BlockItemTag tag;

    union {
        Declaration *decl;
        Statement *stmt;
    };
};

extern BlockItem *blki_declaration(Declaration *decl);
extern BlockItem *blki_statement(Statement *stmt);
extern void blki_free(BlockItem *blki);

//
// AST
//
typedef enum {
    AST_PROGRAM,
    AST_FUNCTION,
} AstTag;

typedef struct {
    AstNode *func;              // function body
} AstProgram;

typedef struct {
    char *name;                 // name of function
    List stmts;                 // of <BlockItem>
} AstFunction;

struct AstNode {
    AstTag tag;
    FileLine loc;               

    union {
        AstProgram prog;        // AST_PROGRAM
        AstFunction func;       // AST_FUNCTION
    };
};

extern AstNode *ast_program(FileLine loc);
extern AstNode *ast_function(char *name, List stmts, FileLine loc);
extern void ast_free(AstNode *ast);

extern void ast_print(AstNode *ast, bool locs);