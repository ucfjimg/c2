#include "ast.h"

#include "ice.h"
#include "safemem.h"

#include <stdio.h>

static void ast_print_recurse(AstNode *ast, int tab, bool locs);
static void exp_print_recurse(Expression *exp, int tab, bool locs);

//
// Allocator for all expression objects.
// 
static Expression *exp_alloc(ExpressionTag tag, FileLine loc)
{
    Expression *exp = safe_zalloc(sizeof(Expression));
    exp->tag = tag;
    exp->loc = loc;
    return exp;
}

//
// Construct an integer constant expression.
//
Expression *exp_int(unsigned long intval, FileLine loc)
{
    Expression *exp = exp_alloc(EXP_INT, loc);
    exp->intval = intval;
    return exp;
}

//
// Construct a variable reference expression.
//
Expression *exp_var(char *name, FileLine loc)
{
    Expression *exp = exp_alloc(EXP_VAR, loc);
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
Expression *exp_unary(UnaryOp op, Expression *exp, FileLine loc)
{
    Expression *uexp = exp_alloc(EXP_UNARY, loc);
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
Expression *exp_binary(BinaryOp op, Expression *left, Expression *right, FileLine loc)
{
    Expression *bexp = exp_alloc(EXP_BINARY, loc);
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
Expression *exp_assignment(BinaryOp op, Expression *left, Expression *right, FileLine loc)
{
    ICE_ASSERT(op == BOP_ASSIGN || bop_is_compound_assign(op));
    Expression *assign = exp_alloc(EXP_ASSIGNMENT, loc);

    assign->assignment.op = op;
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
static Statement *stmt_alloc(StatementTag tag, FileLine loc)
{
    Statement *stmt = safe_zalloc(sizeof(Statement));
    stmt->tag = tag;
    stmt->loc = loc;
    return stmt;
}

//
// Construct a null statement.
//
Statement *stmt_null(FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_NULL, loc);
    return stmt;
}

//
// Construct a declaration statement.
//
Statement *stmt_declaration(char *name, Expression *init, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_DECLARATION, loc);

    stmt->decl.name = safe_strdup(name);
    stmt->decl.init = init;

    return stmt;
}

//
// Free a declaration statement.
//
static void stmt_declaration_free(StmtDeclaration *decl)
{
    safe_free(decl->name);
    exp_free(decl->init);
}

//
// Construct a return statement around an expression (which may be NULL).
//
Statement *stmt_return(Expression *exp, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_RETURN, loc);
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
Statement *stmt_expression(Expression *exp, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_EXPRESSION, loc);
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
static AstNode *ast_alloc(AstTag tag, FileLine loc)
{
    AstNode *ast = safe_zalloc(sizeof(AstNode));
    ast->tag = tag;
    ast->loc = loc;
    return ast;
}

//
// Construct a program with no contents.
//
AstNode *ast_program(FileLine loc)
{
    AstNode *ast = ast_alloc(AST_PROGRAM, loc);
    return ast;
}

//
// Construct a function with the given name but no body.
//
AstNode *ast_function(FileLine loc)
{
    AstNode *ast = ast_alloc(AST_FUNCTION, loc); 
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
// Print an integer constant expression.
//
static void print_exp_const_int(unsigned long val, int tab)
{
    printf("%*sconst-int(%lu);\n", tab, "", val);
}

//
// Print an variable reference expression.
//
static void print_exp_var(char *name, int tab)
{
    printf("%*svar(%s);\n", tab, "", name);
}

//
// Print a unary expression.
//
static void print_exp_unary(ExpUnary *unary, int tab, bool locs)
{
    printf("%*sunary(%s) {\n", tab, "", uop_describe(unary->op));
    exp_print_recurse(unary->exp, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a binary expression.
//
static void print_exp_binary(ExpBinary *binary, int tab, bool locs)
{
    printf("%*sbinary(%s) {\n", tab, "", bop_describe(binary->op));
    exp_print_recurse(binary->left, tab + 2, locs);
    printf("%*s  ,\n", tab, "");
    exp_print_recurse(binary->right, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print an assignment expression.
//
static void print_exp_assignment(ExpAssignment *assign, int tab, bool locs)
{
    printf("%*sassignment(%s) {\n", tab, "", bop_describe(assign->op));
    exp_print_recurse(assign->left, tab + 2, locs);
    printf("%*s}, {\n", tab, "");
    exp_print_recurse(assign->right, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Recusively print an expression, starting at indent `tab`
//
static void exp_print_recurse(Expression *exp, int tab, bool locs)
{
    if (locs) {
        char *loc = fileline_describe(&exp->loc);
        printf("%*s/* %s */\n", tab, "", loc);
        safe_free(loc);
    }

    switch (exp->tag) {
        case EXP_INT:           print_exp_const_int(exp->intval, tab); break;
        case EXP_VAR:           print_exp_var(exp->var.name, tab); break;
        case EXP_UNARY:         print_exp_unary(&exp->unary, tab, locs); break;
        case EXP_BINARY:        print_exp_binary(&exp->binary, tab, locs); break;
        case EXP_ASSIGNMENT:    print_exp_assignment(&exp->assignment, tab, locs); break;
    }
}

//
// Print a variable declaration.
//
static void stmt_print_declaration(StmtDeclaration *decl, int tab, bool locs)
{
    printf("%*sdeclare(%s)", tab, "", decl->name);
    if (decl->init) {
        printf(" = {\n");
        exp_print_recurse(decl->init, tab + 2, locs);
        printf("%*s}", tab, "");
    }
    printf(";\n");
}

//
// Print a null statement.
//
static void stmt_print_null(int tab)
{
    printf("%*snull-statement;\n", tab, "");
}

//
// Print a return statement.
//
static void stmt_print_return(StmtReturn *ret, int tab, bool locs)
{
    printf("%*sreturn {\n", tab, "");
    exp_print_recurse(ret->exp, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a statement expression.
//
static void stmt_print_expression(StmtExpression *exp, int tab, bool locs)
{
    printf("%*sexp {\n", tab, "");
    exp_print_recurse(exp->exp, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Recusively print a statement, starting at indent `tab`
//
static void stmt_print_recurse(Statement *stmt, int tab, bool locs)
{
    if (locs) {
        char *loc = fileline_describe(&stmt->loc);
        printf("%*s/* %s */\n", tab, "", loc);
        safe_free(loc);
    }

    switch (stmt->tag) {
        case STMT_DECLARATION:  stmt_print_declaration(&stmt->decl, tab, locs); break;
        case STMT_NULL:         stmt_print_null(tab); break;
        case STMT_RETURN:       stmt_print_return(&stmt->ret, tab, locs); break;
        case STMT_EXPRESSION:   stmt_print_expression(&stmt->exp, tab, locs); break;
    }
}

//
// Print an entire AST program.
//
static void ast_print_program(AstProgram *prog, int tab, bool locs)
{
    printf("%*sprogram() {\n", tab, "");
    ast_print_recurse(prog->func, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print an AST function.
//
static void ast_print_function(AstFunction *func, int tab, bool locs)
{
    printf("%*sfunction(int, %s) {\n", tab, "", func->name);

    for (ListNode *curr = func->stmts.head; curr; curr = curr->next) {
        stmt_print_recurse(CONTAINER_OF(curr, Statement, list), tab + 2, locs);
    }
    printf("%*s}\n", tab, "");
}

//
// Recursively print an AST, starting at indent `tab`.
//
static void ast_print_recurse(AstNode *ast, int tab, bool locs)
{
    if (locs) {
        char *loc = fileline_describe(&ast->loc);
        printf("%*s/* %s */\n", tab, "", loc);
        safe_free(loc);
    }

    switch (ast->tag) {
        case AST_PROGRAM:   ast_print_program(&ast->prog, tab, locs); break;
        case AST_FUNCTION:  ast_print_function(&ast->func, tab, locs); break;
    }
}

//
// Recursively print an AST. If `locs` is true, also print the file/line
// location of each node.
//
void ast_print(AstNode *ast, bool locs)
{
    ast_print_recurse(ast, 0, locs);
}