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
// Construct a variable reference expression.
//
Expression *exp_var(char *name)
{
    Expression *exp = exp_alloc(EXP_VAR);
    exp->var.name = safe_strdup(name);
    return exp;
}

//
// Free a variable reference expression.
//
static void exp_var_free(ExpVar *var)
{
    safe_free(var->name);
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
// Construct a binary operator.
//
Expression *exp_binary(BinaryOp op, Expression *left, Expression *right)
{
    Expression *bexp = exp_alloc(EXP_BINARY);
    bexp->binary.op = op;
    bexp->binary.left = left;
    bexp->binary.right = right;
 
    return bexp;
}

//
// Free a binary expression.
//
static void exp_binary_free(ExpBinary *binary)
{
    exp_free(binary->left);
    exp_free(binary->right);
}

//
// Construct an assignment expression.
//
Expression *exp_assignment(Expression *left, Expression *right)
{
    Expression *assign = exp_alloc(EXP_ASSIGNMENT);

    assign->assignment.left = left;
    assign->assignment.right = right;

    return assign;
}

//
// Free an assignment expression.
//
void exp_assignment_free(ExpAssignment *assign)
{
    exp_free(assign->left);
    exp_free(assign->right);
}

//
// Free an expression
//
void exp_free(Expression *exp)
{
    if (exp) {
        switch (exp->tag) {
            case EXP_VAR:           exp_var_free(&exp->var); break;
            case EXP_UNARY:         exp_unary_free(&exp->unary); break;
            case EXP_BINARY:        exp_binary_free(&exp->binary); break;
            case EXP_ASSIGNMENT:    exp_assignment_free(&exp->assignment); break;
            default:
                break;
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
// Construct a declaration statement.
//
Statement *stmt_declaration(char *name, Expression *init)
{
    Statement *stmt = stmt_alloc(STMT_DECLARATION);

    stmt->decl.name = safe_strdup(name);
    stmt->decl.init = init;

    return stmt;
}

//
// Free a declaration statement.
//
void stmt_declaration_free(StmtDeclaration *decl)
{
    safe_free(decl->name);
    exp_free(decl->init);
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
static void stmt_return_free(StmtReturn *ret)
{
    exp_free(ret->exp);
}

//
// Construct an expression used as a statement.
//
Statement *stmt_expression(Expression *exp)
{
    Statement *stmt = stmt_alloc(STMT_EXPRESSION);
    stmt->exp.exp = exp;
    return stmt;
}

//
// Free an expression statement
//
static void stmt_expression_free(StmtExpression *exp)
{
    exp_free(exp->exp);
}

//
// Free a statement.
//
void stmt_free(Statement *stmt)
{
    if (stmt) {
        switch (stmt->tag) {
            case STMT_DECLARATION:  stmt_declaration_free(&stmt->decl); break;
            case STMT_RETURN:       stmt_return_free(&stmt->ret); break;
            case STMT_EXPRESSION:   stmt_expression_free(&stmt->exp); break;

            default:
                break;
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

    for (ListNode *curr = func->stmts.head; curr; ) {
        ListNode *next = curr->next;

        stmt_free(CONTAINER_OF(curr, Statement, list));

        curr = next;
    }

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

        case EXP_VAR:
            printf("%svar(%s);\n", indent, exp->var.name);
            break;

        case EXP_UNARY:
            printf("%sunary(%s) {\n", indent, uop_describe(exp->unary.op));
            exp_print_recurse(exp->unary.exp, tab + 2, locs);
            printf("%s}\n", indent);
            break;

        case EXP_BINARY:
            printf("%sbinary(%s) {\n", indent, bop_describe(exp->binary.op));
            exp_print_recurse(exp->binary.left, tab + 2, locs);
            printf("%s  ,\n", indent);
            exp_print_recurse(exp->binary.right, tab + 2, locs);
            printf("%s}\n", indent);
            break;

        case EXP_ASSIGNMENT:
            printf("%sassignment {\n", indent);
            exp_print_recurse(exp->assignment.left, tab + 2, locs);
            printf("%s}, {\n", indent);
            exp_print_recurse(exp->assignment.right, tab + 2, locs);
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
        case STMT_DECLARATION:
            printf("%sdeclare(%s)", indent, stmt->decl.name);
            if (stmt->decl.init) {
                printf(" = {\n");
                exp_print_recurse(stmt->decl.init, tab + 2, locs);
                printf("}");
            }
            printf(";\n");
            break;

        case STMT_NULL:
            printf("%snull-statement;\n", indent);
            break;

        case STMT_RETURN:
            printf("%sreturn {\n", indent);
            exp_print_recurse(stmt->ret.exp, tab + 2, locs);
            printf("%s}\n", indent);
            break;

        case STMT_EXPRESSION:
            printf("%sexp {\n", indent);
            exp_print_recurse(stmt->exp.exp, tab + 2, locs);
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

            for (ListNode *curr = ast->func.stmts.head; curr; curr = curr->next) {
                stmt_print_recurse(CONTAINER_OF(curr, Statement, list), tab + 2, locs);
            }
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