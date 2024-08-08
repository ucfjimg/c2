#include "typecheck.h"

#include "ast.h"
#include "errors.h"
#include "ice.h"
#include "safemem.h"
#include "symtab.h"
#include "type.h"

typedef struct {
    SymbolTable *stab;
    Type *func_type;
} TypeCheckState;

static void ast_check_declaration(TypeCheckState *state, Declaration *decl, bool filescope);
static void ast_check_statement(TypeCheckState *state, Statement *stmt);
static void ast_check_block(TypeCheckState *state, List block);
static Expression* ast_check_expression(TypeCheckState *state, Expression *exp);

//
// Perform the usual and customary arithmetic conversions between two types and
// return the final promoted type.
//
static Type *types_common(Type *left, Type *right)
{
    if (types_equal(left, right)) {
        return type_clone(left);
    }

    int lrank = type_rank(left);
    int rrank = type_rank(right);

    return type_clone(lrank > rrank ? left : right);
}

//
// Safely replace an allocated expression pointer with another.
//
static void exp_replace(Expression **old_exp, Expression *new_exp)
{
    if (*old_exp == new_exp) {
        return;
    }

    exp_free(*old_exp);
    *old_exp = new_exp;
}

//
// If the given expression is not already of the desired type, wrap it in
// a cast expression and return the cast; else, return the original expression.
//
// Unlike many other calls, we do not consume the passed type here, as 
// it is unclear to the caller if the type object is used or not.
//
static Expression *convert_to(Expression *exp, Type *type)
{
    if (types_equal(exp->type, type)) {
        return exp;
    }

    return exp_cast(type_clone(type), exp, exp->loc);
}

//
// Type check an integer constant.
//
static Expression *ast_check_int(TypeCheckState *state, Expression *exp)
{
    exp_set_type(exp, type_int());
    return exp;
}

//
// Type check a long constant.
//
static Expression *ast_check_long(TypeCheckState *state, Expression *exp)
{
    exp_set_type(exp, type_long());
    return exp;
}

//
// Type check an unsgined integer constant.
//
static Expression *ast_check_uint(TypeCheckState *state, Expression *exp)
{
    exp_set_type(exp, type_uint());
    return exp;
}

//
// Type check an unsigned long constant.
//
static Expression *ast_check_ulong(TypeCheckState *state, Expression *exp)
{
    exp_set_type(exp, type_ulong());
    return exp;
}

//
// Type check a floating point constant.
//
static Expression *ast_check_float(TypeCheckState *state, Expression *exp)
{
    exp_set_type(exp, type_double());
    return exp;
}

//
// Type check a variable reference.
//
static Expression *ast_check_var(TypeCheckState *state, Expression *exp)
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

    exp_set_type(exp, type_clone(sym->type ? sym->type : type_int()));
    return exp;
}

//
// Type check a unary expression.
//
static Expression *ast_check_unary(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_UNARY);
    ExpUnary *unary = &exp->unary;

    exp_replace(&unary->exp, ast_check_expression(state, unary->exp));

    if (unary->op == UOP_COMPLEMENT && unary->exp->type->tag == TT_DOUBLE) {
        err_report(EC_ERROR, &exp->loc, "can only apply `~` to integers.");
    }
    
    //
    // The NOT operator returns an int of 0 or 1. All the other 
    // unary operators are type preserving.
    //
    if (unary->op == UOP_LOGNOT) {
        exp_set_type(exp, type_int());
    } else {
        exp_set_type(exp, type_clone(unary->exp->type));
    }

    return exp;
}

//
// Type check a binary expression which converts the operands to a common
// type.
//
static Expression *ast_check_binary_promote(TypeCheckState *state, Expression *exp, bool only_int)
{
    ICE_ASSERT(exp->tag == EXP_BINARY);
    ExpBinary *binary = &exp->binary;

    Type *common = types_common(binary->left->type, binary->right->type);

    if (only_int && common->tag == TT_DOUBLE) {
        err_report(EC_ERROR, &exp->loc, "neither operand to operator `%s` may be floating point.", bop_describe(binary->op));
    }

    //
    // Do not use exp_replace here as the original expression object is
    // guaranteed to still be in use.
    //
    binary->left = convert_to(binary->left, common);
    binary->right = convert_to(binary->right, common);

    exp_set_type(exp, common);
    return exp;
}

//
// Type check a binary expression which takes the type of the left-hand operand,
// and which must have integral operands. (i.e. shifts).
//
static Expression *ast_check_binary_lhs_int(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_BINARY);
    ExpBinary *binary = &exp->binary;

    if (binary->left->type->tag == TT_DOUBLE || binary->right->type->tag == TT_DOUBLE) {
        err_report(EC_ERROR, &exp->loc, "neither operand to operator `%s` may be floating point.", bop_describe(binary->op));
    }

    exp_set_type(exp, type_clone(binary->left->type));
    return exp;
}

//
// Type check a binary expression which requires the operands to be promoted to
// the same but, but which returns an integer (anything that returns a Boolean
// result, like the relational operators).
//
static Expression *ast_check_binary_bool(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_BINARY);
    ExpBinary *binary = &exp->binary;

    Type *common = types_common(binary->left->type, binary->right->type);

    //
    // Do not use exp_replace here as the original expression object is
    // guaranteed to still be in use.
    //
    binary->left = convert_to(binary->left, common);
    binary->right = convert_to(binary->right, common);

    type_free(common);

    exp_set_type(exp, type_int());
    return exp; 
}

//
// Type check a binary expression.
//
static Expression *ast_check_binary(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_BINARY);
    ExpBinary *binary = &exp->binary;
        
    exp_replace(&binary->left, ast_check_expression(state, binary->left));
    exp_replace(&binary->right, ast_check_expression(state, binary->right));

    switch (binary->op) {
        case BOP_ADD:
        case BOP_SUBTRACT:
        case BOP_MULTIPLY:
        case BOP_DIVIDE:        return ast_check_binary_promote(state, exp, false);

        case BOP_DIVDBL:        ICE_NYI("ast_check_binary::BOP_DIVDBL");

        case BOP_MODULO:
        case BOP_BITAND:
        case BOP_BITOR:
        case BOP_BITXOR:        return ast_check_binary_promote(state, exp, true);

        case BOP_LSHIFT:
        case BOP_RSHIFT:        return ast_check_binary_lhs_int(state, exp);

        case BOP_LOGAND:
        case BOP_LOGOR:
        case BOP_EQUALITY:
        case BOP_NOTEQUAL:
        case BOP_LESSTHAN:
        case BOP_GREATERTHAN:
        case BOP_LESSEQUAL:
        case BOP_GREATEREQUAL:  return ast_check_binary_bool(state, exp);

        //
        // It is a bug for any of these operators to be in a binary
        // operator expression; they are special cases handled elsewhere.
        //
        case BOP_CONDITIONAL:
        case BOP_ASSIGN:
        case BOP_COMPOUND_ADD:
        case BOP_COMPOUND_SUBTRACT:
        case BOP_COMPOUND_MULTIPLY:
        case BOP_COMPOUND_DIVIDE:
        case BOP_COMPOUND_MODULO:
        case BOP_COMPOUND_BITAND:
        case BOP_COMPOUND_BITOR:
        case BOP_COMPOUND_BITXOR:
        case BOP_COMPOUND_LSHIFT:
        case BOP_COMPOUND_RSHIFT:
            break;
    }       

    ICE_ASSERT(((void)"invalid binary operator in AST in ast_check_binary", false));
    return NULL;
}

//
// Type check a conditional expression.
//
static Expression *ast_check_conditional(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_CONDITIONAL);
    ExpConditional *cond = &exp->conditional;

    exp_replace(&cond->cond, ast_check_expression(state, cond->cond));
    exp_replace(&cond->trueval, ast_check_expression(state, cond->trueval));
    exp_replace(&cond->falseval, ast_check_expression(state, cond->falseval));

    Type *common = types_common(cond->trueval->type, cond->falseval->type);

    //
    // Do not use exp_replace here as the original expression object is
    // guaranteed to still be in use.
    //
    cond->trueval = convert_to(cond->trueval, common);
    cond->falseval = convert_to(cond->falseval, common);

    exp_set_type(exp, common);
    return exp;
}

//
// Type check an assignment.
//
static Expression *ast_check_assignment(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_ASSIGNMENT);
    ExpAssignment *assign = &exp->assignment;

    exp_replace(&assign->left, ast_check_expression(state, assign->left));
    exp_replace(&assign->right, ast_check_expression(state, assign->right));

    assign->right = convert_to(assign->right, assign->left->type);

    exp_set_type(exp, type_clone(assign->left->type));
    return exp;
}

//
// Type check a function call.
//
static Expression *ast_check_function_call(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_FUNCTION_CALL);
    ExpFunctionCall *call = &exp->call;

    Symbol *sym = stab_lookup(state->stab, call->name);
    
    //
    // If type is NULL, we are calling an undeclared function (which has already
    // been reported.)
    //
    if (sym->type == NULL) {
        exp_set_type(exp, type_int());
        return exp;
    }

    //
    // Make sure it's a function.
    //
    if (sym->type->tag != TT_FUNC) {
        err_report(EC_ERROR, &exp->loc, "attempt to call `%s` which is not of function type.", call->name);
        exp_set_type(exp, type_int());
        return exp;
    } 

    //
    // Type convert arguments.
    //
    ListNode *argcurr = call->args.head;
    ListNode *parmcurr = sym->type->func.parms.head;

    List new_args;
    list_clear(&new_args);

    while (argcurr && parmcurr) {
        ListNode *argnext = argcurr->next;

        Expression *arg = CONTAINER_OF(argcurr, Expression, list);    
        TypeFuncParam *parm = CONTAINER_OF(parmcurr, TypeFuncParam, list);

        exp_replace(&arg, ast_check_expression(state, arg));
        arg = convert_to(arg, parm->parmtype);

        list_push_back(&new_args, &arg->list);

        argcurr = argnext;
        parmcurr = parmcurr->next;
    }

    call->args = new_args;

    if (argcurr || parmcurr) {
        err_report(EC_ERROR, &exp->loc, "incorrect number of arguments for function `%s`.",
            call->name);
    }

    exp_set_type(exp, type_clone(sym->type->func.ret));
    return exp;
}

//
// Type check a cast expression.
//
static Expression *ast_check_cast(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_CAST);
    ExpCast *cast = &exp->cast;

    exp_replace(&cast->exp, ast_check_expression(state, cast->exp));

    exp_set_type(exp, type_clone(cast->type));
    return exp;
}

//
// Type check an expression, and return the type checked expression (which may be
// the same object, or a new object).
//
static Expression* ast_check_expression(TypeCheckState *state, Expression *exp)
{
    switch (exp->tag) {
        case EXP_INT:           return ast_check_int(state, exp); break;
        case EXP_LONG:          return ast_check_long(state, exp); break;
        case EXP_UINT:          return ast_check_uint(state, exp); break;
        case EXP_ULONG:         return ast_check_ulong(state, exp); break;
        case EXP_FLOAT:         return ast_check_float(state, exp); break;
        case EXP_VAR:           return ast_check_var(state, exp); break;
        case EXP_UNARY:         return ast_check_unary(state, exp); break;
        case EXP_BINARY:        return ast_check_binary(state, exp); break;
        case EXP_CONDITIONAL:   return ast_check_conditional(state, exp); break;
        case EXP_ASSIGNMENT:    return ast_check_assignment(state, exp); break;
        case EXP_FUNCTION_CALL: return ast_check_function_call(state, exp); break;
        case EXP_CAST:          return ast_check_cast(state, exp); break;
    }

    ICE_ASSERT(((void)"invalid expression tag in ast_check_expression", false));
    return NULL;
}

//
// Type check a return statement.
//
static void ast_check_return(TypeCheckState *state, StmtReturn *ret)
{
    exp_replace(&ret->exp, ast_check_expression(state, ret->exp));

    ret->exp = convert_to(ret->exp, type_clone(state->func_type->func.ret));
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
    bool global = func->func.storage_class != SC_STATIC;
    bool had_body = false;

    if (sym->type) {
        if (sym->type->tag != TT_FUNC) {
            err_report(EC_ERROR, &func->loc, "invalid redeclaration of symbol `%s`.", func->func.name);
        }

        if (sym->func.defined && func->func.has_body) {
            err_report(EC_ERROR, &func->loc, "invalid redefinition of function `%s`.", func->func.name);
        }

        if (sym->func.global && !global) {
            err_report(EC_ERROR, &func->loc, "non-static function `%s` may not be redeclared as static.", func->func.name);
        }

        if (!types_equal(sym->type, func->func.type)) {
            err_report(EC_ERROR, &func->loc, "function `%s` redeclared with different type.", func->func.name);
        }

        had_body = sym->type->tag == TT_FUNC && sym->func.defined;
        global = sym->func.global;
    } else {
        sym->type = type_clone(func->func.type);
    }

    sym_update_func(sym, sym->type, had_body || func->func.has_body, global);

    Type *old_func_type = state->func_type;
    state->func_type = sym->type;

    if (func->func.has_body) {
        ICE_ASSERT(func->func.type->tag == TT_FUNC);

        ListNode *pcurr = func->func.parms.head;
        ListNode *tcurr = func->func.type->func.parms.head;

        for (; pcurr && tcurr; pcurr = pcurr->next, tcurr = tcurr->next) {
            FuncParameter *parm = CONTAINER_OF(pcurr, FuncParameter, list);
            TypeFuncParam *ptype = CONTAINER_OF(tcurr, TypeFuncParam, list);

            Symbol *sym = stab_lookup(state->stab, parm->name);

            if (sym->type == NULL) {
                sym_update_local(sym, type_clone(ptype->parmtype));
            }
        }

        ICE_ASSERT(pcurr == NULL && tcurr == NULL);

        ast_check_block(state, func->func.body);
    }

    state->func_type = old_func_type;
}

//
// Convert a constant to a type.
//
static Const ast_convert_const_to(Const *cn, Type *ty)
{
    Const out = *cn;

    if (cn->tag == CON_INTEGRAL && ty->tag == TT_DOUBLE) {
        out.tag = CON_FLOAT;
        if (cn->intval.sign == CIS_SIGNED) {
            out.floatval = (double)(long)cn->intval.value;
        } else {
            out.floatval = (double)cn->intval.value;
        }
    }

    if (cn->tag == CON_FLOAT) {
        switch (ty->tag) {
            case TT_INT:    out = const_make_int(CIS_INT,  CIS_SIGNED,   (int)cn->floatval); break;
            case TT_UINT:   out = const_make_int(CIS_INT,  CIS_UNSIGNED, (unsigned)cn->floatval); break;
            case TT_LONG:   out = const_make_int(CIS_LONG, CIS_SIGNED,   (long)cn->floatval); break;
            case TT_ULONG:  out = const_make_int(CIS_LONG, CIS_UNSIGNED, (unsigned long)cn->floatval); break;
            case TT_DOUBLE: break;
                
            case TT_FUNC: ICE_ASSERT(((void)"cannot convert function in ast_convert_const_to", false));
        }
    }

    return out;
}

//
// Type check a file scope variable declaration.
//
static void ast_check_global_var_decl(TypeCheckState *state, Declaration *decl)
{
    ICE_ASSERT(decl->tag == DECL_VARIABLE);
    DeclVariable *var = &decl->var;

    StaticInitialValue siv = SIV_NO_INIT;

    Const init = const_make_int(CIS_INT, CIS_SIGNED, 0);

    if (var->init == NULL) {
        if (var->storage_class == SC_EXTERN) {
            siv = SIV_NO_INIT;
        } else {
            siv = SIV_TENTATIVE;
        }
    } else if (
        var->init->tag == EXP_INT || 
        var->init->tag == EXP_LONG || 
        var->init->tag == EXP_UINT || 
        var->init->tag == EXP_ULONG ||
        var->init->tag == EXP_FLOAT) {

        siv = SIV_INIT;

        switch (var->init->tag) {
            case EXP_INT:       init = const_make_int(CIS_INT, CIS_SIGNED, (int)var->init->intval); break;
            case EXP_UINT:      init = const_make_int(CIS_INT, CIS_UNSIGNED, (unsigned)var->init->intval); break;
            case EXP_LONG:      init = const_make_int(CIS_LONG, CIS_SIGNED, var->init->intval); break;
            case EXP_ULONG:     init = const_make_int(CIS_LONG, CIS_UNSIGNED, var->init->intval); break;
            case EXP_FLOAT:     init = const_make_double(var->init->floatval); break;

            //
            // Checked above
            //
            default:
                break;
        }
    } else {
        err_report(EC_ERROR, &decl->loc, "non-constant initializer not allowed for static `%s`.", var->name);
    }

    bool globally_visible = var->storage_class != SC_STATIC;

    Symbol *sym = stab_lookup(state->stab, var->name);
    Type *type = sym->type;

    if (sym->type) {
        if (!types_equal(var->type, sym->type)) {
            err_report(EC_ERROR, &decl->loc, "symbol `%s` redefined as different type.", var->name);
            return;
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
        if (sym->stvar.siv == SIV_INIT) {
            if (var->init) {
                err_report(EC_ERROR, &decl->loc, "symbol `%s` has conflicting declarations.", var->name);
            } else {
                siv = sym->stvar.siv;
                init = sym->stvar.initial;
            }
        } else if (var->init == NULL && sym->stvar.siv == SIV_TENTATIVE) {
            siv = SIV_TENTATIVE;
        }
    } else {
        type = type_clone(var->type);
    }

    init = ast_convert_const_to(&init, type);
    sym_update_static_var(sym, type, siv, init, globally_visible, decl->loc);
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
    
    if (sym->type && !types_equal(sym->type, var->type)) {
        err_report(EC_ERROR, &decl->loc, "cannot redeclare variable `%s` as different type.", var->name);
    }

    Type *type = type_clone(var->type);

    Const init = const_make_zero();
    
    if (var->storage_class == SC_EXTERN) {
        if (var->init != NULL) {
            err_report(EC_ERROR, &decl->loc, "local extern variable `%s` may not have an initializer.", var->name);
        }

        if (sym->type) {
            if (sym->type->tag == TT_FUNC) {
                err_report(EC_ERROR, &decl->loc, "cannot redeclare function `%s` as a local extern.", var->name);
            }
            type_free(type);
        } else {
            init = ast_convert_const_to(&init, type);
            sym_update_static_var(sym, type, SIV_NO_INIT, init, true, decl->loc);
        }
    } else if (var->storage_class == SC_STATIC) {
        if (var->init == NULL) {
            init = ast_convert_const_to(&init, type);
            sym_update_static_var(sym, type, SIV_INIT, init, false, decl->loc);
        } else {
            switch (var->init->tag) {
                case EXP_INT:   init = const_make_int(CIS_INT, CIS_SIGNED, (int)var->init->intval); break;
                case EXP_UINT:  init = const_make_int(CIS_INT, CIS_UNSIGNED, (unsigned)var->init->intval); break;
                case EXP_LONG:  init = const_make_int(CIS_LONG, CIS_SIGNED, var->init->intval); break;
                case EXP_ULONG: init = const_make_int(CIS_LONG, CIS_UNSIGNED, var->init->intval); break;
                case EXP_FLOAT: init = const_make_double(var->init->floatval); break;

                default:
                    err_report(EC_ERROR, &decl->loc, "static initializer for `%s` must be a constant.", var->name);
            }
            init = ast_convert_const_to(&init, type);
            sym_update_static_var(sym, type, SIV_INIT, init, false, decl->loc);
        }
    } else {
        //
        // No storage class
        //
        sym_update_local(sym, type);
        if (var->init) {
            ast_check_expression(state, var->init);
            var->init = convert_to(var->init, var->type);
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
