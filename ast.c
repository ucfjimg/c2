#include "ast.h"

#include "safemem.h"
#include "strutil.h"

#include <stdio.h>

//
// Allocator for all expression objects.
// 
static Expression *exp_alloc(ExpressionTag tag)
{
    Expression *exp = safe_zalloc(sizeof(Expression));
    exp->tag = tag;
    return exp;
}

//
// Construct an integer constant expression.
//
Expression *exp_int(unsigned long intval)
{
    Expression *exp = exp_alloc(EXP_INT);
    exp->intval = intval;
    return exp;
}

//
// Construct a unary operator.
//
Expression *exp_unary(UnaryOp op, Expression *exp)
{
    Expression *uexp = exp_alloc(EXP_UNARY);
    uexp->unary.op = op;
    uexp->unary.exp = exp;
    return uexp;
}

//
// Free a unary expression.
//
static void exp_unary_free(ExpUnary *unary)
{
    exp_free(unary->exp);
}

//
// Free an expression
//
void exp_free(Expression *exp)
{
    if (exp) {
        switch (exp->tag) {
            case EXP_UNARY: exp_unary_free(&exp->unary); break;
        }

        safe_free(exp);
    }
}

//
// Allocatorfor all statement objects.
//
static Statement *stmt_alloc(StatementTag tag)
{
    Statement *stmt = safe_zalloc(sizeof(Statement));
    stmt->tag = tag;
    return stmt;
}

//
// Construct a null statement.
//
Statement *stmt_null(void)
{
    Statement *stmt = stmt_alloc(STMT_NULL);
    return stmt;
}

//
// Construct a return statement around an expression (which may be NULL).
//
Statement *stmt_return(Expression *exp)
{
    Statement *stmt = stmt_alloc(STMT_RETURN);
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
// Allocator for all AST nodes.
//
AstNode *ast_alloc(AstTag tag)
{
    AstNode *ast = safe_zalloc(sizeof(AstNode));
    ast->tag = tag;
    return ast;
}

//
// Construct a program with no contents.
//
AstNode *ast_program(void)
{
    AstNode *ast = ast_alloc(AST_PROGRAM);
    return ast;
}

//
// Construct a function with the given name but no body.
//
AstNode *ast_function(void)
{
    AstNode *ast = ast_alloc(AST_FUNCTION); 
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

//
// Recusively print an expression, starting at indent `tab`
//
static void exp_print_recurse(Expression *exp, int tab, bool locs)
{
    char *indent = str_repeat(tab, ' ');

    if (locs) {
        char *loc = fileline_describe(&exp->loc);
        printf("%s/* %s */\n", indent, loc);
        safe_free(loc);
    }

    switch (exp->tag) {
        case EXP_INT:
            printf("%sconst-int(%lu);\n", indent, exp->intval);
            break;

        case EXP_UNARY:
            printf("%sunary(%s) {\n", indent, uop_describe(exp->unary.op));
            exp_print_recurse(exp->unary.exp, tab + 2, locs);
            printf("%s}\n", indent);
            break;
    }

    safe_free(indent);
}

//
// Recusively print a statement, starting at indent `tab`
//
static void stmt_print_recurse(Statement *stmt, int tab, bool locs)
{
    char *indent = str_repeat(tab, ' ');

    if (locs) {
        char *loc = fileline_describe(&stmt->loc);
        printf("%s/* %s */\n", indent, loc);
        safe_free(loc);
    }

    switch (stmt->tag) {
        case STMT_NULL:
            printf("%snull-statement;\n", indent);
            break;

        case STMT_RETURN:
            printf("%sreturn {\n", indent);
            exp_print_recurse(stmt->ret.exp, tab + 2, locs);
            printf("%s}\n", indent);
            break;
    }

    safe_free(indent);
}


//
// Recursively print an AST, starting at indent `tab`.
//
static void ast_print_recurse(AstNode *ast, int tab, bool locs)
{
    char *indent = str_repeat(tab, ' ');

    if (locs) {
        char *loc = fileline_describe(&ast->loc);
        printf("%s/* %s */\n", indent, loc);
        safe_free(loc);
    }

    switch (ast->tag) {
        case AST_PROGRAM:
            printf("%sprogram() {\n", indent);
            ast_print_recurse(ast->prog.func, tab + 2, locs);
            printf("%s}\n", indent);
            break;

        case AST_FUNCTION:
            printf("%sfunction(int, %s) {\n", indent, ast->func.name);
            stmt_print_recurse(ast->func.stmt, tab + 2, locs);
            printf("%s}\n", indent);
            break;
    }


    safe_free(indent);
}

//
// Recursively print an AST. If `locs` is true, also print the file/line
// location of each node.
//
void ast_print(AstNode *ast, bool locs)
{
    ast_print_recurse(ast, 0, locs);
}