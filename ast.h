#pragma once

typedef struct Expression Expression;
typedef struct Statement Statement;
typedef struct AstNode AstNode;

//
// expressions
//
typedef enum { 
    EXP_INT,
} ExpressionTag;

struct Expression {
    ExpressionTag tag;

    union {
        unsigned long intval;
    };
};

extern Expression *exp_int(unsigned long intval);
extern void exp_free(Expression *exp);

//
// statemements
//
typedef enum {
    STMT_NULL,
    STMT_RETURN,
} StatementTag;

typedef struct {
    Expression *exp;            // return value (may be NULL) 
} StmtReturn;

struct Statement {
    StatementTag tag;

    union {
        StmtReturn ret;         // STMT_RETURN
    };
};

extern Statement *stmt_null(void);
extern Statement *stmt_return(Expression *exp);
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
    Statement *stmt;            // function body
} AstFunction;

struct AstNode {
    AstTag tag;

    union {
        AstProgram prog;        // AST_PROGRAM
        AstFunction func;       // AST_FUNCTION
    };
};

extern AstNode *ast_program(void);
extern AstNode *ast_function(void);
extern void ast_free(AstNode *ast);

extern void ast_print(AstNode *ast);