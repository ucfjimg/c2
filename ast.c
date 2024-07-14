#include "ast.h"

#include "safemem.h"

//
// Construct an integer constant expression
//
Expression *exp_int(unsigned long intval)
{
    Expression *exp = safe_zalloc(sizeof(Expression));

    exp->tag = EXP_INT;
    exp->intval = intval;

    return exp;
}

//
// Free an expression
//
void exp_free(Expression *exp)
{
    if (exp) {
        safe_free(exp);
    }
}

//
// Construct a null statement.
//
Statement *stmt_null(void)
{
    Statement *stmt = safe_zalloc(sizeof(Statement));

    stmt->tag = STMT_NULL;

    return stmt;
}

//
// Construct a return statement around an expression (which may be NULL).
//
Statement *stmt_return(Expression *exp)
{
    Statement *stmt = safe_zalloc(sizeof(Statement));

    stmt->tag = STMT_RETURN;
    stmt->ret.exp = exp;

    return stmt;
}

//
// Free a return statement
//
void stmt_return_free(StmtReturn *ret)
{
    exp_free(ret->exp);
}

//
// Free a statement.
//
void stmt_free(Statement *stmt)
{
    if (stmt) {
        switch (stmt->tag) {
            case STMT_RETURN: stmt_return_free(&stmt->ret); break;
        }

        safe_free(stmt);
    }
}


//
// Construct a program with no contents.
//
AstNode *ast_program(void)
{
    AstNode *ast = safe_zalloc(sizeof(AstNode));

    ast->tag = AST_PROGRAM;

    return ast;
}

//
// Construct a function with the given name but no body.
//
AstNode *ast_function(void)
{
    AstNode *ast = safe_zalloc(sizeof(AstNode));

    ast->tag = AST_FUNCTION;
 
    return ast;
}

//
// Free a program
//
static void ast_free_program(AstProgram *prog)
{
    ast_free(prog->func);
}

//
// Free a function
//
static void ast_free_function(AstFunction *func)
{
    safe_free(func->name);
    stmt_free(func->stmt);
}

//
// Free an AST node.
//
void ast_free(AstNode *ast)
{
    if (ast) {
        switch (ast->tag) {
            case AST_PROGRAM: ast_free_program(&ast->prog); break;
            case AST_FUNCTION: ast_free_function(&ast->func); break;
        }

        safe_free(ast);
    }
}

