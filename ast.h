#pragma once

#include "fileline.h"
#include "list.h"
#include "operators.h"

#include <stdbool.h>

typedef struct Expression Expression;
typedef struct Statement Statement;
typedef struct AstNode AstNode;

//
// expressions
//
typedef enum { 
    EXP_INT,
    EXP_VAR,
    EXP_UNARY,
    EXP_BINARY,
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
        ExpAssignment assignment;
    };
};

extern Expression *exp_int(unsigned long intval, FileLine loc);
extern Expression *exp_var(char *name, FileLine loc);
extern Expression *exp_unary(UnaryOp op, Expression *exp, FileLine loc);
extern Expression *exp_binary(BinaryOp op, Expression *left, Expression *right, FileLine loc);
extern Expression *exp_assignment(Expression *left, Expression *right, FileLine loc);
extern void exp_free(Expression *exp);

//
// statemements
//
typedef enum {
    STMT_DECLARATION,
    STMT_NULL,
    STMT_RETURN,
    STMT_EXPRESSION,
} StatementTag;

typedef struct {
    char *name;                 // name of variable being declared
    Expression *init;           // initializatiom (may be NULL)
} StmtDeclaration;

typedef struct {
    Expression *exp;            // return value (may be NULL) 
} StmtReturn;

typedef struct {
    Expression *exp;            // expression used as statement
} StmtExpression;

struct Statement {
    ListNode list;
    StatementTag tag;
    FileLine loc;               

    union {
        StmtDeclaration decl;   // STMT_DECLARATION
        StmtReturn ret;         // STMT_RETURN
        StmtExpression exp;     // STMT_EXPRESSION
    };
};

extern Statement *stmt_declaration(char *name, Expression *init, FileLine loc);
extern Statement *stmt_null(FileLine loc);
extern Statement *stmt_return(Expression *exp, FileLine loc);
extern Statement *stmt_expression(Expression *exp, FileLine loc);
extern void stmt_free(Statement *stmt);

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
    List stmts;                 // of <Statement>
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
extern AstNode *ast_function(FileLine loc);
extern void ast_free(AstNode *ast);

extern void ast_print(AstNode *ast, bool locs);