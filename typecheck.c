#include "typecheck.h"

#include "ast.h"
#include "errors.h"
#include "ice.h"
#include "symtab.h"

typedef struct {
    SymbolTable *stab;
} TypeCheckState;

static void ast_check_declaration(TypeCheckState *state, Declaration *decl, bool filescope);
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
        case FI_DECLARATION:    ast_check_declaration(state, for_->init->decl, false); break;
        case FI_EXPRESSION:     ast_check_expression(state, for_->init->exp); break;
    }

    if (for_->init->tag == FI_DECLARATION) {
        Declaration *decl = for_->init->decl;
        //
        // Parser already validated this.
        //
        ICE_ASSERT(decl->tag == DECL_VARIABLE);

        if (decl->var.storage_class != SC_NONE) {
            err_report(EC_ERROR, &decl->loc, "for loop initializer declaration may not have a storage class.");
        }
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
            case BI_DECLARATION:    ast_check_declaration(state, blki->decl, false); break;
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
    bool global = func->func.storage_class != SC_STATIC;
    bool had_body = false;
    Type *type = NULL;

    if (sym->type) {
        if (sym->type->tag != TT_FUNC || sym->type->func.parms != parm_count) {
            err_report(EC_ERROR, &func->loc, "invalid redeclaration of symbol `%s`.", func->func.name);
        }

        if (sym->func.defined && func->func.has_body) {
            err_report(EC_ERROR, &func->loc, "invalid redefinition of function `%s`.", func->func.name);
        }

        if (sym->func.global && !global) {
            err_report(EC_ERROR, &func->loc, "non-static function `%s` may not be redeclared as static.", func->func.name);
        }

        had_body = sym->type->tag == TT_FUNC && sym->func.defined;
        type = sym->type;
        global = sym->func.global;
    } else {
        type = type_function(parm_count); 
    }

    sym_update_func(sym, type, had_body || func->func.has_body, global);

    if (func->func.has_body) {
        for (ListNode *curr = func->func.parms.head; curr; curr = curr->next) {
            FuncParameter *parm = CONTAINER_OF(curr, FuncParameter, list);
            Symbol *sym = stab_lookup(state->stab, parm->name);
            //
            // if type is already not NULL, it means a duplicate identifier in the parm
            // list, which we will have already reported in the resolve pass.
            //
            if (sym->type == NULL) {
                sym_update_local(sym, type_int());
            }
        }

        ast_check_block(state, func->func.body);
    }
}

//
// Type check a file scope variable declaration.
//
static void ast_check_global_var_decl(TypeCheckState *state, Declaration *decl)
{
    ICE_ASSERT(decl->tag == DECL_VARIABLE);
    DeclVariable *var = &decl->var;

    StaticInitialValue init_value = SIV_NO_INIT;
    unsigned long init_const = 0;

    if (var->init == NULL) {
        if (var->storage_class == SC_EXTERN) {
            init_value = SIV_NO_INIT;
        } else {
            init_value = SIV_TENTATIVE;
        }
    } else if (var->init->tag == EXP_INT) {
        init_const = var->init->intval;
    } else {
        err_report(EC_ERROR, &decl->loc, "non-constant initializer not allowed for static `%s`.", var->name);
    }

    bool globally_visible = var->storage_class != SC_STATIC;

    Symbol *sym = stab_lookup(state->stab, var->name);

    if (sym->type) {
        if (sym->type->tag == TT_FUNC) {
            err_report(EC_ERROR, &decl->loc, "symbol `%s` redefined as different type.", var->name);
        }

        if (var->storage_class == SC_EXTERN) {
            //
            // externs keep their old visibility
            //
            globally_visible = sym->stvar.global;
        } else if (globally_visible != sym->stvar.global) {
            //
            // Otherwise, can't change visibility.
            //
            err_report(EC_ERROR, &decl->loc, "symbol `%s` redefined as different visibility.", var->name);
        }

        //
        // Make sure not trying to have two initialized declarations.
        //
        if (sym->stvar.explicit_init) {
            if (var->init) {
                err_report(EC_ERROR, &decl->loc, "symbol `%s` has conflicting declarations.", var->name);
            } else {
                init_const = sym->stvar.initial;
            }
        } else if (var->init == NULL && sym->stvar.siv == SIV_TENTATIVE) {
            init_value = SIV_TENTATIVE;
        }
    }

    sym_update_static_var(sym, type_int(), init_value, init_const, var->init != NULL, globally_visible, decl->loc);
}

//
// Type check a variable declaration.
//
static void ast_check_var_decl(TypeCheckState *state, Declaration *decl, bool filescope)
{
    if (filescope) {
        ast_check_global_var_decl(state, decl);
        return;
    }

    ICE_ASSERT(decl->tag == DECL_VARIABLE);
    DeclVariable *var = &decl->var;

    Symbol *sym = stab_lookup(state->stab, var->name);

    if (var->storage_class == SC_EXTERN) {
        if (var->init != NULL) {
            err_report(EC_ERROR, &decl->loc, "local extern variable `%s` may not have an initializer.", var->name);
        }

        if (sym->type) {
            if (sym->type->tag == TT_FUNC) {
                err_report(EC_ERROR, &decl->loc, "cannot redeclare function `%s` as a local extern.", var->name);
            }
        } else {
            sym_update_static_var(sym, type_int(), SIV_NO_INIT, 0, false, true, decl->loc);
        }
    } else if (var->storage_class == SC_STATIC) {
        if (var->init == NULL) {
            sym_update_static_var(sym, type_int(), SIV_INIT, 0, false, false, decl->loc);
        } else if (var->init->tag == EXP_INT) {
            sym_update_static_var(sym, type_int(), SIV_INIT, var->init->intval, true, false, decl->loc);
        } else {
            err_report(EC_ERROR, &decl->loc, "static initializer for `%s` must be a constant.", var->name);
            sym_update_static_var(sym, type_int(), SIV_INIT, 0, false, false, decl->loc);
        }
    } else {
        //
        // No storage class
        //
        sym_update_local(sym, type_int());
        if (var->init) {
            ast_check_expression(state, var->init);
        }
    }
}

//
// Type check a declaration.
//
static void ast_check_declaration(TypeCheckState *state, Declaration *decl, bool filescope)
{
    switch (decl->tag) {
        case DECL_FUNCTION: ast_check_func_decl(state, decl); break;
        case DECL_VARIABLE: ast_check_var_decl(state, decl, filescope); break;
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
        ast_check_declaration(&state, decl, true);
    } 
}
