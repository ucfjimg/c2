#include "typecheck.h"

#include "ast.h"
#include "errors.h"
#include "ice.h"
#include "symtab.h"

typedef struct {
    SymbolTable *stab;
} TypeCheckState;

static void ast_check_declaration(TypeCheckState *state, Declaration *decl);
static void ast_check_statement(TypeCheckState *state, Statement *stmt);
static void ast_check_block(TypeCheckState *state, List block);
static void ast_check_expression(TypeCheckState *state, Expression *exp);

//
// Type check a variable reference.
//
static void ast_check_var(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_VAR);
    ExpVar *var = &exp->var;

    Symbol *sym = stab_lookup(state->stab, var->name);
    //
    // if the type is NULL, we already reported an undeclared error in the resolve pass.
    //
    if (sym->type) {
        if (sym->type->tag == TT_FUNC) {
            err_report(EC_ERROR, &exp->loc, "cannot use function `%s` as variable.", var->name);
        }
    }
}

//
// Type check a unary expression.
//
static void ast_check_unary(TypeCheckState *state, ExpUnary *unary)
{
    ast_check_expression(state, unary->exp);
}

//
// Type check a binary expression.
//
static void ast_check_binary(TypeCheckState *state, ExpBinary *binary)
{
    ast_check_expression(state, binary->left);
    ast_check_expression(state, binary->right);
}

//
// Type check a conditional expression.
//
static void ast_check_conditional(TypeCheckState *state, ExpConditional *cond)
{
    ast_check_expression(state, cond->cond);
    ast_check_expression(state, cond->trueval);
    ast_check_expression(state, cond->falseval);
}

//
// Type check an assignment.
//
static void ast_check_assignment(TypeCheckState *state, ExpAssignment *assign)
{
    ast_check_expression(state, assign->left);
    ast_check_expression(state, assign->right);
}

//
// Type check a function call.
//
static void ast_check_function_call(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_FUNCTION_CALL);
    ExpFunctionCall *call = &exp->call;

    Symbol *sym = stab_lookup(state->stab, call->name);
    
    //
    // If type is NULL, we are calling an undeclared function (which has already
    // been reported.)
    //
    if (sym->type == NULL) {
        return;
    }

    //
    // Make sure it's a function.
    //
    if (sym->type->tag != TT_FUNC) {
        err_report(EC_ERROR, &exp->loc, "attempt to call `%s` which is not of function type.", call->name);
        return;
    } 

    //
    // Make sure the argument list matches. TODO with real types, type check each arg
    // against the matching parameter.
    //
    int arg_count = list_count(&call->args);
    if (arg_count != sym->type->func.parms) {
        err_report(EC_ERROR, &exp->loc, "incorrect number of arguments for function `%s`; got %d, expected %d.",
            call->name,
            arg_count,
            sym->type->func.parms);
    }

    //
    // Validate argument expressions.
    //
    for (ListNode *curr = call->args.head; curr; curr = curr->next) {
        Expression *arg = CONTAINER_OF(curr, Expression, list);
        ast_check_expression(state, arg);
    }
}

//
// Type check an expression.
//
static void ast_check_expression(TypeCheckState *state, Expression *exp)
{
    switch (exp->tag) {
        case EXP_INT:           break;
        case EXP_VAR:           ast_check_var(state, exp); break;
        case EXP_UNARY:         ast_check_unary(state, &exp->unary); break;
        case EXP_BINARY:        ast_check_binary(state, &exp->binary); break;
        case EXP_CONDITIONAL:   ast_check_conditional(state, &exp->conditional); break;
        case EXP_ASSIGNMENT:    ast_check_assignment(state, &exp->assignment); break;
        case EXP_FUNCTION_CALL: ast_check_function_call(state, exp); break;
    }
}

//
// Type check a return statement.
//
static void ast_check_return(TypeCheckState *state, StmtReturn *ret)
{
    ast_check_expression(state, ret->exp);
}

//
// Type check an if statement.
//
static void ast_check_if(TypeCheckState *state, StmtIf *ifelse)
{
    ast_check_expression(state, ifelse->condition);
    ast_check_statement(state, ifelse->thenpart);
    if (ifelse->elsepart) {
        ast_check_statement(state, ifelse->elsepart);
    }
}

//
// Type check a label.
//
static void ast_check_label(TypeCheckState *state, StmtLabel *label)
{
    ast_check_statement(state, label->stmt);
}

//
// Type check a compound statement.
//
static void ast_check_compound(TypeCheckState *state, StmtCompound *compound)
{
    ast_check_block(state, compound->items);
}

//
// Type check a while loop.
//
static void ast_check_while(TypeCheckState *state, StmtWhile *while_)
{
    ast_check_expression(state, while_->cond);
    ast_check_statement(state, while_->body);
}

//
// Type check a for loop.
//
static void ast_check_for(TypeCheckState *state, StmtFor *for_)
{
    switch (for_->init->tag) {
        case FI_NONE:           break;
        case FI_DECLARATION:    ast_check_declaration(state, for_->init->decl); break;
        case FI_EXPRESSION:     ast_check_expression(state, for_->init->exp); break;
    }

    if (for_->cond) {
        ast_check_expression(state, for_->cond);
    }

    if (for_->post) {
        ast_check_expression(state, for_->post);
    }

    ast_check_statement(state, for_->body);
}

//
// Type check a do while loop.
//
static void ast_check_do_while(TypeCheckState *state, StmtDoWhile *dowhile)
{
    ast_check_expression(state, dowhile->cond);
    ast_check_statement(state, dowhile->body);
}

//
// Type check a switch statement.
//
static void ast_check_switch(TypeCheckState *state, StmtSwitch *switch_)
{
    ast_check_expression(state, switch_->cond);
    ast_check_statement(state, switch_->body);
}

//
// Type check a case statement.
//
static void ast_check_case(TypeCheckState *state, StmtCase *case_)
{
    ast_check_statement(state, case_->stmt);
}

//
// Type check a default statement.
//
static void ast_check_default(TypeCheckState *state, StmtDefault *default_)
{
    ast_check_statement(state, default_->stmt);
}

//
// Type check a statement.
//
static void ast_check_statement(TypeCheckState *state, Statement *stmt)
{
    switch (stmt->tag) {
        case STMT_NULL:         break;
        case STMT_RETURN:       ast_check_return(state, &stmt->ret); break;
        case STMT_IF:           ast_check_if(state, &stmt->ifelse); break;
        case STMT_EXPRESSION:   ast_check_expression(state, stmt->exp.exp); break;
        case STMT_LABEL:        ast_check_label(state, &stmt->label); break;
        case STMT_GOTO:         break;
        case STMT_COMPOUND:     ast_check_compound(state, &stmt->compound); break;
        case STMT_WHILE:        ast_check_while(state, &stmt->while_); break;
        case STMT_FOR:          ast_check_for(state, &stmt->for_); break;
        case STMT_DOWHILE:      ast_check_do_while(state, &stmt->dowhile); break;
        case STMT_BREAK:        break;
        case STMT_CONTINUE:     break;
        case STMT_SWITCH:       ast_check_switch(state, &stmt->switch_); break;
        case STMT_CASE:         ast_check_case(state, &stmt->case_); break;
        case STMT_DEFAULT:      ast_check_default(state, &stmt->default_); break;
    }
}

//
// Type check a block.
//
static void ast_check_block(TypeCheckState *state, List block)
{
    for (ListNode *curr = block.head; curr; curr = curr->next) {
        BlockItem *blki = CONTAINER_OF(curr, BlockItem, list);
        switch (blki->tag) {
            case BI_DECLARATION:    ast_check_declaration(state, blki->decl); break;
            case BI_STATEMENT:      ast_check_statement(state, blki->stmt); break;
        }
    }
}

//
// Type check a function declaration.
//
static void ast_check_func_decl(TypeCheckState *state, Declaration *func)
{
    ICE_ASSERT(func->tag == DECL_FUNCTION);

    Symbol *sym = stab_lookup(state->stab, func->func.name);
    int parm_count = list_count(&func->func.parms);

    if (sym->type) {
        if (sym->type->tag != TT_FUNC || sym->type->func.parms != parm_count) {
            err_report(EC_ERROR, &func->loc, "invalid redeclaration of symbol `%s`.", func->func.name);
        }

        if (sym->defined && func->func.has_body) {
            err_report(EC_ERROR, &func->loc, "invalid redefinition of function `%s`.", func->func.name);
        }
    } else {
        sym->type = type_function(parm_count); 
    }

    if (func->func.has_body) {
        sym->defined = true;

        for (ListNode *curr = func->func.parms.head; curr; curr = curr->next) {
            FuncParameter *parm = CONTAINER_OF(curr, FuncParameter, list);
            Symbol *sym = stab_lookup(state->stab, parm->name);
            //
            // if type is already not NULL, it means a duplicate identifier in the parm
            // list, which we will have already reported in the resolve pass.
            //
            if (sym->type == NULL) {
                sym->type = type_int();
            }
        }

        ast_check_block(state, func->func.body);
    }
}

//
// Type check a variable declaration.
//
static void ast_check_var_decl(TypeCheckState *state, Declaration *var)
{
    ICE_ASSERT(var->tag == DECL_VARIABLE);

    Symbol *sym = stab_lookup(state->stab, var->var.name);
    //
    // If the type is not NULL, it means a duplicate declaration, which we will
    // have already reported in the resolve pass.
    //
    if (sym->type == NULL) {
        sym->type = type_int();
    }

    if (var->var.init) {
        ast_check_expression(state, var->var.init);
    }
}

//
// Type check a declaration.
//
static void ast_check_declaration(TypeCheckState *state, Declaration *decl)
{
    switch (decl->tag) {
        case DECL_FUNCTION: ast_check_func_decl(state, decl); break;
        case DECL_VARIABLE: ast_check_var_decl(state, decl); break;
    }
}

//
// Type check an entire program.
//
void ast_typecheck(AstProgram *prog, SymbolTable *stab)
{
    TypeCheckState state;

    state.stab = stab;

    for (ListNode *curr = prog->decls.head; curr; curr = curr->next) {
        Declaration *decl = CONTAINER_OF(curr, Declaration, list);
        ast_check_declaration(&state, decl);
    } 
}