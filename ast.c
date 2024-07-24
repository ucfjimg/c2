#include "ast.h"

#include "ice.h"
#include "safemem.h"

#include <stdio.h>

static void ast_print_recurse(AstNode *ast, int tab, bool locs);
static void stmt_print_recurse(Statement *ast, int tab, bool locs);
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
// Construct a conditional operator (i.e. a ? b : c).
//
Expression *exp_conditional(Expression *conditional, Expression *trueval, Expression *falseval, FileLine loc)
{
    Expression *cexp = exp_alloc(EXP_CONDITIONAL, loc);

    cexp->conditional.cond = conditional;
    cexp->conditional.trueval = trueval;
    cexp->conditional.falseval = falseval;

    return cexp;
}

//
// Free a conditional expression.
//
static void exp_conditional_free(ExpConditional *cond)
{
    exp_free(cond->cond);
    exp_free(cond->trueval);
    exp_free(cond->falseval);
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
            case EXP_CONDITIONAL:   exp_conditional_free(&exp->conditional); break;
            case EXP_ASSIGNMENT:    exp_assignment_free(&exp->assignment); break;
            default:
                break;
        }

        safe_free(exp);
    }
}

//
// Constructor for a declaration.
//
// Note that `init` is optional and will be NULL if the declaration
// has no initializer.
// 
Declaration *declaration(char *name, Expression *init, FileLine loc)
{
    Declaration *decl = safe_zalloc(sizeof(Declaration));

    decl->loc = loc;
    decl->name = safe_strdup(name);
    decl->init = init;

    return decl;
}

//
// Free a declaration.
//
void declaration_free(Declaration *decl)
{
    if (decl) {
        safe_free(decl->name);
        exp_free(decl->init);
    
        safe_free(decl);
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
// Construct a return statement around an expression (which may be NULL).
//
Statement *stmt_return(Expression *exp, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_RETURN, loc);
    stmt->ret.exp = exp;
    return stmt;
}

//
// Free a return statement.
//
static void stmt_return_free(StmtReturn *ret)
{
    exp_free(ret->exp);
}

//
// Construct an if statement.
//
Statement *stmt_if(Expression *condition, Statement *thenpart, Statement *elsepart, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_IF, loc);
    stmt->ifelse.condition = condition;
    stmt->ifelse.thenpart = thenpart;
    stmt->ifelse.elsepart = elsepart;

    return stmt;

}

//
// Free an if statement.
// 
static void stmt_if_free(StmtIf *ifelse)
{
    exp_free(ifelse->condition);
    stmt_free(ifelse->thenpart);
    stmt_free(ifelse->elsepart);
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
// Free an expression statement.
//
static void stmt_expression_free(StmtExpression *exp)
{
    exp_free(exp->exp);
}

//
// Constructor for a label statment (e.g. `label:`).
//
Statement *stmt_label(char *name, Statement *labeled_stmt, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_LABEL, loc);
    stmt->label.name = safe_strdup(name);
    stmt->label.stmt = labeled_stmt;
    return stmt;
}

//
// Free a label statement.
//
static void stmt_label_free(StmtLabel *label)
{
    safe_free(label->name);
    stmt_free(label->stmt);
}

//
// Constructor for a goto statement.
//
Statement *stmt_goto(char *target, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_GOTO, loc);
    stmt->goto_.target = safe_strdup(target);
    return stmt;
}

//
// Free a goto statement.
//
static void stmt_goto_free(StmtGoto *goto_)
{
    safe_free(goto_->target);
}

//
// Constructor for a compound statement.
//
Statement *stmt_compound(List items, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_COMPOUND, loc);
    stmt->compound.items = items;
    return stmt;
}

//
// Free a compound statement.
//
static void stmt_compound_free(StmtCompound *compound)
{
    for (ListNode *curr = compound->items.head; curr; ) {
        ListNode *next = curr->next;
        blki_free(CONTAINER_OF(curr, BlockItem, list));
        curr = next;
    }
}

//
// Constructor for an empty for initializer.
//
ForInit *forinit(void)
{
    ForInit *fi = safe_zalloc(sizeof(ForInit));
    fi->tag = FI_NONE;
    return fi;
}

//
// Constructor for a for initializer with a expression.
//
ForInit *forinit_exp(Expression *exp)
{
    ForInit *fi = forinit();

    fi->tag = FI_EXPRESSION;
    fi->exp = exp;

    return fi;
}

//
// Constructor for a for initializer with a declaration.
//
ForInit *forinit_decl(Declaration *decl)
{
    ForInit *fi = forinit();

    fi->tag = FI_DECLARATION;
    fi->decl = decl;

    return fi;
}

//
// Free a for loop initializer.
//
void forinit_free(ForInit *fi)
{
    if (fi) {
        switch (fi->tag) {
            case FI_EXPRESSION:     exp_free(fi->exp); break;
            case FI_DECLARATION:    declaration_free(fi->decl); break;
            case FI_NONE:           break;
        }
    }
}

//
// Constuctor for a for loop.
//
Statement *stmt_for(ForInit *init, Expression *cond, Expression *post, Statement *body, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_FOR, loc);

    stmt->for_.init = init;
    stmt->for_.cond = cond;
    stmt->for_.post = post;
    stmt->for_.body = body;
    stmt->for_.label = -1;

    return stmt;
}

//
// Free a for loop.
//
static void stmt_for_free(StmtFor *for_) 
{
    forinit_free(for_->init);
    exp_free(for_->cond);
    exp_free(for_->post);
    stmt_free(for_->body);
}

//
// Constructor for a while loop.
//
Statement *stmt_while(Expression *cond, Statement *body, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_WHILE, loc);

    stmt->while_.cond = cond;
    stmt->while_.body = body;
    stmt->while_.label = -1;

    return stmt;
}

//
// Free a while loop.
//
void stmt_while_free(StmtWhile *while_)
{
    exp_free(while_->cond);
    stmt_free(while_->body);
}

//
// Constructor for a do-while loop.
//
Statement *stmt_do_while(Expression *cond, Statement *body, FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_DOWHILE, loc);

    stmt->dowhile.cond = cond;
    stmt->dowhile.body = body;
    stmt->dowhile.label = -1;

    return stmt;
}

//
// Free a do-while loop.
//
void stmt_do_while_free(StmtDoWhile *dowhile)
{
    exp_free(dowhile->cond);
    stmt_free(dowhile->body);
}

//
// Constructor for a break statement.
//
Statement *stmt_break(FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_BREAK, loc);
    stmt->break_.label = -1;
    return stmt;
}

//
// Constructor for a continue statement.
//
Statement *stmt_continue(FileLine loc)
{
    Statement *stmt = stmt_alloc(STMT_CONTINUE, loc);
    stmt->continue_.label = -1;
    return stmt;
}

//
// Free a statement.
//
void stmt_free(Statement *stmt)
{
    if (stmt) {
        switch (stmt->tag) {
            case STMT_RETURN:       stmt_return_free(&stmt->ret); break;
            case STMT_IF:           stmt_if_free(&stmt->ifelse); break;
            case STMT_EXPRESSION:   stmt_expression_free(&stmt->exp); break;
            case STMT_NULL:         break;
            case STMT_LABEL:        stmt_label_free(&stmt->label); break;
            case STMT_GOTO:         stmt_goto_free(&stmt->goto_); break;
            case STMT_COMPOUND:     stmt_compound_free(&stmt->compound); break;
            case STMT_FOR:          stmt_for_free(&stmt->for_); break;
            case STMT_WHILE:        stmt_while_free(&stmt->while_); break;
            case STMT_DOWHILE:      stmt_do_while_free(&stmt->dowhile); break;
            case STMT_BREAK:        break;
            case STMT_CONTINUE:     break;
        }

        safe_free(stmt);
    }
}

//
// Allocator for all BlockItem nodes.
//
static BlockItem *blki_alloc(BlockItemTag tag)
{
    BlockItem *blki = safe_zalloc(sizeof(BlockItem));
    blki->tag = tag;
    return blki;
}

//
// Constructor for a declaration block item.
//
BlockItem *blki_declaration(Declaration *decl)
{
    BlockItem *blki = blki_alloc(BI_DECLARATION);
    blki->decl = decl;
    return blki;
}

//
// Constructor for a statement block item.
//
BlockItem *blki_statement(Statement *stmt)
{
    BlockItem *blki = blki_alloc(BI_STATEMENT);
    blki->stmt = stmt;
    return blki;
}

//
// Free a block item.
//
void blki_free(BlockItem *blki)
{
    if (blki) {
        switch (blki->tag) {
            case BI_DECLARATION:    declaration_free(blki->decl); break;
            case BI_STATEMENT:      stmt_free(blki->stmt); break;
        }
        safe_free(blki);
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
// Free a program.
//
static void ast_free_program(AstProgram *prog)
{
    ast_free(prog->func);
}

//
// Construct a function with the given name but no body.
//
AstNode *ast_function(char *name, List stmts, FileLine loc)
{
    AstNode *ast = ast_alloc(AST_FUNCTION, loc);
    ast->func.name = safe_strdup(name);
    ast->func.stmts =  stmts;
    return ast;
}

//
// Free a function.
//
static void ast_free_function(AstFunction *func)
{
    safe_free(func->name);

    for (ListNode *curr = func->stmts.head; curr; ) {
        ListNode *next = curr->next;

        blki_free(CONTAINER_OF(curr, BlockItem, list));

        curr = next;
    }
}

//
// Construct compound statement.
//

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
// Print a conditional expression.
//
static void print_exp_conditional(ExpConditional *cond, int tab, bool locs)
{
    printf("%*sconditional {\n", tab, "");
    exp_print_recurse(cond->cond, tab + 2, locs);
    printf("%*s} ? {\n", tab, "");
    exp_print_recurse(cond->trueval, tab + 2, locs);
    printf("%*s} : {\n", tab, "");
    exp_print_recurse(cond->falseval, tab + 2, locs);
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
        case EXP_CONDITIONAL:   print_exp_conditional(&exp->conditional, tab, locs); break;
        case EXP_ASSIGNMENT:    print_exp_assignment(&exp->assignment, tab, locs); break;
    }
}

//
// Print a variable declaration.
//
static void decl_print_declaration(Declaration *decl, int tab, bool locs)
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
// Print an if statement.
//
static void stmt_print_if(StmtIf *ifelse, int tab, bool locs)
{
    printf("%*sif (\n", tab, "");
    exp_print_recurse(ifelse->condition, tab + 2, locs);
    printf("%*s) {\n", tab, "");
    stmt_print_recurse(ifelse->thenpart, tab + 2, locs);
    if (ifelse->elsepart) {
        printf("%*s} else {\n", tab, "");
        stmt_print_recurse(ifelse->elsepart, tab + 2, locs);
    }
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
// Print a label statement.
//
static void stmt_print_label(StmtLabel *label, int tab, bool locs)
{
    printf("%*slabel(%s) {\n", tab, "", label->name);
    stmt_print_recurse(label->stmt, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a goto statement.
//
static void stmt_print_goto(StmtGoto *goto_, int tab)
{
    printf("%*sgoto(%s);\n", tab, "", goto_->target);
}

//
// Print a compound statement.
//
static void stmt_print_compound(StmtCompound *compound, int tab, bool locs)
{
    printf("%*scompound {\n", tab, "");
    for (ListNode *curr = compound->items.head; curr; curr = curr->next) {
        BlockItem *blki = CONTAINER_OF(curr, BlockItem, list);
        switch (blki->tag) {
            case BI_DECLARATION:    decl_print_declaration(blki->decl, tab + 2, locs); break;
            case BI_STATEMENT:      stmt_print_recurse(blki->stmt, tab + 2, locs); break;
        }
    }
    printf("%*s}\n", tab, "");
}

//
// Print a for initializer.
//
static void stmt_print_forinit(ForInit *fi, int tab, bool locs)
{
    switch (fi->tag) {
        case FI_EXPRESSION:     exp_print_recurse(fi->exp, tab, locs); break;
        case FI_DECLARATION:    decl_print_declaration(fi->decl, tab, locs); break;
        case FI_NONE:           break;
    }
}

//
// Print a for loop.
//
static void stmt_print_for(StmtFor *for_, int tab, bool locs)
{
    printf("%*sfor(%d) {\n", tab, "", for_->label);
    stmt_print_forinit(for_->init, tab + 2, locs);
    printf("%*s} ; {\n", tab, "");
    if (for_->cond) {
        exp_print_recurse(for_->cond, tab + 2, locs);
    }
    printf("%*s} ; {\n", tab, "");
    if (for_->post) {
        exp_print_recurse(for_->post, tab + 2, locs);
    }
    printf("%*s} {\n", tab, "");
    stmt_print_recurse(for_->body, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a while loop.
//
static void stmt_print_while(StmtWhile *while_, int tab, bool locs)
{
    printf("%*swhile(%d) {\n", tab, "", while_->label);
    exp_print_recurse(while_->cond, tab + 2, locs);
    printf("%*s} {\n", tab, "");
    stmt_print_recurse(while_->body, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a do-while loop.
//
static void stmt_print_do_while(StmtDoWhile *dowhile, int tab, bool locs)
{
    printf("%*sdo(%d) {\n", tab, "", dowhile->label);
    stmt_print_recurse(dowhile->body, tab + 2, locs);
    printf("%*s} while {\n", tab, "");
    exp_print_recurse(dowhile->cond, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a break statement.
//
static void stmt_print_break(StmtBreak *break_, int tab)
{
    printf("%*sbreak(%d);\n", tab, "", break_->label);
}

//
// Print a continue statement.
//
static void stmt_print_continue(StmtContinue *continue_, int tab)
{
    printf("%*scontinue(%d);\n", tab, "", continue_->label);
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
        case STMT_NULL:         stmt_print_null(tab); break;
        case STMT_RETURN:       stmt_print_return(&stmt->ret, tab, locs); break;
        case STMT_IF:           stmt_print_if(&stmt->ifelse, tab, locs); break;
        case STMT_EXPRESSION:   stmt_print_expression(&stmt->exp, tab, locs); break;
        case STMT_LABEL:        stmt_print_label(&stmt->label, tab, locs); break;
        case STMT_GOTO:         stmt_print_goto(&stmt->goto_, tab); break;
        case STMT_COMPOUND:     stmt_print_compound(&stmt->compound, tab, locs); break;
        case STMT_FOR:          stmt_print_for(&stmt->for_, tab, locs); break;
        case STMT_WHILE:        stmt_print_while(&stmt->while_, tab, locs); break;
        case STMT_DOWHILE:      stmt_print_do_while(&stmt->dowhile, tab, locs); break;
        case STMT_BREAK:        stmt_print_break(&stmt->break_, tab); break;
        case STMT_CONTINUE:     stmt_print_continue(&stmt->continue_, tab); break;
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
// Print a block (list of BlockItem's).
//
static void ast_print_blockitem_list(List block, int tab, bool locs)
{
    for (ListNode *curr = block.head; curr; curr = curr->next) {
        BlockItem *blki = CONTAINER_OF(curr, BlockItem, list);
        switch (blki->tag) {
            case BI_DECLARATION:    decl_print_declaration(blki->decl, tab + 2, locs); break;
            case BI_STATEMENT:      stmt_print_recurse(blki->stmt, tab + 2, locs);
        }
    }
}

//
// Print an AST function.
//
static void ast_print_function(AstFunction *func, int tab, bool locs)
{
    printf("%*sfunction(int, %s) {\n", tab, "", func->name);
    ast_print_blockitem_list(func->stmts, tab, locs);
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