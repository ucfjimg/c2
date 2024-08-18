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
    AstState *ast;
} TypeCheckState;

static void ast_check_declaration(TypeCheckState *state, Declaration *decl, bool filescope);
static void ast_check_statement(TypeCheckState *state, Statement *stmt);
static void ast_check_block(TypeCheckState *state, List block);
static Expression* ast_check_expression(TypeCheckState *state, Expression *exp);
static Initializer *make_zero_init(TypeCheckState *state, Type *type, FileLine loc);

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

    *old_exp = new_exp;
}

//
// If the given expression is not already of the desired type, wrap it in
// a cast expression and return the cast; else, return the original expression.
//
// Unlike many other calls, we do not consume the passed type here, as 
// it is unclear to the caller if the type object is used or not.
//
static Expression *convert_to(TypeCheckState *state, Expression *exp, Type *type)
{
    if (types_equal(exp->type, type)) {
        return exp;
    }

    Expression *cast = exp_cast(state->ast, type_clone(type), exp, exp->loc);
    cast->type = type_clone(type);
    return cast;
} 

//
// Return true if the given expression is an l-value.
//
static bool exp_is_lvalue(Expression *exp)
{
    return
        exp->tag == EXP_VAR ||
        exp->tag == EXP_DEREF || 
        exp->tag == EXP_SUBSCRIPT;
}

//
// Return true if the given expression can be used in a NULL pointer
// context.
//
static bool exp_is_null_pointer(Expression *exp)
{
    return
        (exp->tag == EXP_INT && exp->intval == 0) ||
        (exp->tag == EXP_LONG && exp->longval == 0) ||
        (exp->tag == EXP_UINT && exp->uintval == 0) ||
        (exp->tag == EXP_ULONG && exp->ulongval == 0);
}

//
// Implement the rules described in the standard as "convert as if by assigment."
//
static Expression *convert_by_assignment(TypeCheckState *state, Expression *exp, Type *type)
{
    if (types_equal(exp->type, type)) {
        return exp;
    }

    if (type_arithmetic(exp->type) && type_arithmetic(type)) {
        return convert_to(state, exp, type);
    }

    if (exp_is_null_pointer(exp) && type->tag == TT_POINTER) {
        return convert_to(state, exp, type);
    }

    err_report(EC_ERROR, &exp->loc, "invalid type conversion.");

    return exp;
}

//
// Return the common pointer types of two expressions, if there is one.
//
static Type *ptr_type_common(Expression *left, Expression *right)
{
    if (types_equal(left->type, right->type)) {
        return type_clone(left->type);
    }

    if (exp_is_null_pointer(left)) {
        return type_clone(right->type);
    }

    if (exp_is_null_pointer(right)) {
        return type_clone(left->type);
    }

    err_report(EC_ERROR, &left->loc, "incompatible pointer types.");
    return type_clone(left->type);
}

//
// Typecheck an expression, and then apply array-to-pointer decay as needed.
//
static Expression *typecheck_and_convert(TypeCheckState *state, Expression *exp)
{
    Expression *typed_exp = ast_check_expression(state, exp);

    if (typed_exp->type->tag == TT_ARRAY) {
        Expression *addrof = exp_addrof(state->ast, exp, exp->loc);
        exp_set_type(addrof, type_pointer(type_clone(typed_exp->type->array.element)));
        return addrof;
    }

    return typed_exp;
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

    exp_replace(&unary->exp, typecheck_and_convert(state, unary->exp));

    if (
        unary->op == UOP_COMPLEMENT && (unary->exp->type->tag == TT_DOUBLE || unary->exp->type->tag == TT_POINTER)) {
        err_report(EC_ERROR, &exp->loc, "can only apply `~` to integers.");
    } 

    if (unary->op == UOP_MINUS && unary->exp->type->tag == TT_POINTER) {        
        err_report(EC_ERROR, &exp->loc, "cannot apply `-` to pointers.");
    }

    bool incdec = 
        unary->op == UOP_PREINCREMENT || 
        unary->op == UOP_POSTINCREMENT || 
        unary->op == UOP_PREDECREMENT || 
        unary->op == UOP_POSTDECREMENT; 

    if (incdec && (!type_arithmetic(unary->exp->type) || unary->exp->type->tag == TT_POINTER)) {
        err_report(EC_ERROR, &exp->loc, "++/-- may only be applied to numeric or pointer types.");
    }

    if (incdec && unary->exp->tag != EXP_VAR && unary->exp->tag != EXP_DEREF) {
        err_report(EC_ERROR, &exp->loc, "++/-- may only be applied to l-values.");
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
    binary->left = convert_to(state, binary->left, common);
    binary->right = convert_to(state, binary->right, common);

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

    if (type_unsigned(binary->left->type) && binary->op == BOP_RSHIFT) {
        binary->op = BOP_URSHIFT;
    }

    exp_set_type(exp, type_clone(binary->left->type));
    return exp;
}

//
// Type check a binary expression which returns an integer (anything that 
// returns a Boolean result, like the relational operators).
//
static Expression *ast_check_binary_bool(TypeCheckState *state, Expression *exp, bool promote)
{
    ICE_ASSERT(exp->tag == EXP_BINARY);
    ExpBinary *binary = &exp->binary;

    if (promote) {
        Type *common = NULL;
        
        common = types_common(binary->left->type, binary->right->type);

        //
        // Do not use exp_replace here as the original expression object is
        // guaranteed to still be in use.
        //
        binary->left = convert_to(state, binary->left, common);
        binary->right = convert_to(state, binary->right, common);

        type_free(common);
    }

    exp_set_type(exp, type_int());
    return exp; 
}

//
// Type check a binary equality expression between two pointers.
//
static Expression *ast_check_pointer_equality(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_BINARY);
    ExpBinary *binary = &exp->binary;

    Type *common = ptr_type_common(binary->left, binary->right);

    //
    // Do not use exp_replace here as the original expression object is
    // guaranteed to still be in use.
    //
    binary->left = convert_to(state, binary->left, common);
    binary->right = convert_to(state, binary->right, common);

    type_free(common);

    exp_set_type(exp, type_int());
    return exp; 
}

//
// Type check addition that involves pointers.
//
static Expression *ast_check_add_pointers(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_BINARY);
    ExpBinary *binary = &exp->binary;
    ICE_ASSERT(binary->op == BOP_ADD);    

    //
    // The caller has verified that at least one operand is a pointer.
    //
    if (type_integral(binary->right->type)) {
        //
        // pointer + integer
        //
        exp_replace(&binary->right, convert_to(state, binary->right, type_long()));
        exp_set_type(exp, type_clone(binary->left->type));
    } else if (type_integral(binary->left->type)) {
        //
        // integer + pointer
        //
        exp_replace(&binary->left, convert_to(state, binary->left, type_long()));
        exp_set_type(exp, type_clone(binary->right->type));
    } else {
        err_report(EC_ERROR, &exp->loc, "invalid operands for `+`.");
        exp_set_type(exp, type_int());
    }

    return exp;
}

//
// Type check subtraction that involves pointers.
//
static Expression *ast_check_sub_pointers(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_BINARY);
    ExpBinary *binary = &exp->binary;
    ICE_ASSERT(binary->op == BOP_SUBTRACT);    

    //
    // The caller has verified that at least one operand is a pointer.
    //
    if (type_integral(binary->right->type)) {
        //
        // pointer - integer
        //
        exp_replace(&binary->right, convert_to(state, binary->right, type_long()));
        exp_set_type(exp, type_clone(binary->left->type));
    } else if (binary->left->type->tag == TT_POINTER && binary->right->type->tag == TT_POINTER) {
        if (!types_equal(binary->left->type, binary->right->type)) {
            err_report(EC_ERROR, &exp->loc, "cannot subtract pointers of different types.");
        }
        //
        // pointer - pointer
        //
        exp_set_type(exp, type_long());
    } else {
        err_report(EC_ERROR, &exp->loc, "invalid operands for `-`.");
        exp_set_type(exp, type_int());
    }

    return exp;
}

//
// Type check a non-equality pointer relationship.
//
static Expression *ast_check_compare_pointers(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_BINARY);
    ExpBinary *binary = &exp->binary;

    exp_set_type(exp, type_int());

    if (binary->left->type->tag != TT_POINTER || binary->right->type->tag != TT_POINTER) {
        err_report(EC_ERROR, &exp->loc, "invalid operands for `%s`", bop_describe(binary->op));
    }

    if (!types_equal(binary->left->type, binary->right->type)) {
        err_report(EC_ERROR, &exp->loc, "cannot compare pointers of different types.");
    }

    return exp;
}

//
// Type check a binary expression.
//
static Expression *ast_check_binary(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_BINARY);
    ExpBinary *binary = &exp->binary;
        
    exp_replace(&binary->left, typecheck_and_convert(state, binary->left));
    exp_replace(&binary->right, typecheck_and_convert(state, binary->right));

    bool pointers = binary->left->type->tag == TT_POINTER || binary->right->type->tag == TT_POINTER;

    if (pointers) {
        switch (binary->op) {
            case BOP_EQUALITY:
            case BOP_NOTEQUAL:  return ast_check_pointer_equality(state, exp);

            case BOP_LOGAND:
            case BOP_LOGOR:     return ast_check_binary_bool(state, exp, false);

            case BOP_MULTIPLY:
            case BOP_DIVIDE:
            case BOP_DIVDBL:
            case BOP_MODULO:
                err_report(EC_ERROR, &exp->loc, "cannot apply multiplicative operator to a pointer.");
                exp_set_type(exp, type_int());
                return exp;

            case BOP_BITAND:
            case BOP_BITOR:
            case BOP_BITXOR:
            case BOP_LSHIFT:
            case BOP_RSHIFT:
            case BOP_URSHIFT:
                err_report(EC_ERROR, &exp->loc, "cannot apply bitwise operator to a pointer.");
                exp_set_type(exp, type_int());
                return exp;


            case BOP_ADD:       return ast_check_add_pointers(state, exp);
            case BOP_SUBTRACT:  return ast_check_sub_pointers(state, exp);

            case BOP_LESSTHAN:
            case BOP_GREATERTHAN:
            case BOP_LESSEQUAL:
            case BOP_GREATEREQUAL:
                                return ast_check_compare_pointers(state, exp);

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
            case BOP_COMPOUND_URSHIFT:
                break;
        }
    } else {
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
            case BOP_RSHIFT:
            case BOP_URSHIFT:       return ast_check_binary_lhs_int(state, exp);

            case BOP_LOGAND:
            case BOP_LOGOR:         return ast_check_binary_bool(state, exp, false);

            case BOP_EQUALITY:
            case BOP_NOTEQUAL:
            case BOP_LESSTHAN:
            case BOP_GREATERTHAN:
            case BOP_LESSEQUAL:
            case BOP_GREATEREQUAL:  return ast_check_binary_bool(state, exp, true);

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
            case BOP_COMPOUND_URSHIFT:
                break;
        }       
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

    exp_replace(&cond->cond, typecheck_and_convert(state, cond->cond));
    exp_replace(&cond->trueval, typecheck_and_convert(state, cond->trueval));
    exp_replace(&cond->falseval, typecheck_and_convert(state, cond->falseval));

    Type *common;
    
    if (cond->trueval->type->tag == TT_POINTER || cond->falseval->type->tag == TT_POINTER) {
        common = ptr_type_common(cond->trueval, cond->falseval); 
    } else {
        common = types_common(cond->trueval->type, cond->falseval->type);
    }

    //
    // Do not use exp_replace here as the original expression object is
    // guaranteed to still be in use.
    //
    cond->trueval = convert_to(state, cond->trueval, common);
    cond->falseval = convert_to(state, cond->falseval, common);

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

    exp_replace(&assign->left, typecheck_and_convert(state, assign->left));
    exp_replace(&assign->right, typecheck_and_convert(state, assign->right));

    if (!exp_is_lvalue(assign->left)) {
        err_report(EC_ERROR, &exp->loc, "l-value required for assignment.");
    }

    assign->right = convert_by_assignment(state, assign->right, assign->left->type);

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

        exp_replace(&arg, typecheck_and_convert(state, arg));
        arg = convert_by_assignment(state, arg, parm->parmtype);

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

    exp_replace(&cast->exp, typecheck_and_convert(state, cast->exp));

    if (
        (cast->type->tag == TT_POINTER && cast->exp->type->tag == TT_DOUBLE) ||
        (cast->type->tag == TT_DOUBLE && cast->exp->type->tag == TT_POINTER)) {
        
        err_report(EC_ERROR, &exp->loc, "cannot cast between a double and a pointer.");
    }

    if (cast->type->tag == TT_ARRAY) {
        err_report(EC_ERROR, &exp->loc, "cannot cast to an array type.");
    }

    exp_set_type(exp, type_clone(cast->type));
    return exp;
}

//
// Type check a dereference expression.
//
static Expression *ast_check_deref(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_DEREF);
    ExpDeref *deref = &exp->deref;

    exp_replace(&deref->exp, typecheck_and_convert(state, deref->exp));

    if (deref->exp->type->tag == TT_POINTER) {
        exp_set_type(exp, type_clone(deref->exp->type->ptr.ref));
    } else {
        err_report(EC_ERROR, &exp->loc, "cannot dereference non-pointer.");
        exp_set_type(exp, type_int());
    }

    return exp;
}

//
// Type check an addr-of expression.
//
static Expression *ast_check_addrof(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_ADDROF);
    ExpAddrOf *addrof = &exp->addrof;

    exp_replace(&addrof->exp, ast_check_expression(state, addrof->exp));

    if (exp_is_lvalue(addrof->exp)) {
        exp_set_type(exp, type_pointer(type_clone(addrof->exp->type)));
    } else {
        err_report(EC_ERROR, &exp->loc, "cannot take the address of something which is not an l-value.");
        exp_set_type(exp, type_int());
    }

    return exp;
}

//
// Type check a subscript expression.
//
static Expression *ast_check_subscript(TypeCheckState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_SUBSCRIPT);
    ExpSubscript *subs = &exp->subscript;

    exp_replace(&subs->left, typecheck_and_convert(state, subs->left));
    exp_replace(&subs->right, typecheck_and_convert(state, subs->right));

    if (subs->left->type->tag == TT_POINTER && type_integral(subs->right->type)) {
        exp_replace(&subs->right, convert_to(state, subs->right, type_long()));
        exp_set_type(exp, subs->left->type->ptr.ref);        
    } else if (type_integral(subs->left->type) && subs->right->type->tag == TT_POINTER) {
        exp_replace(&subs->left, convert_to(state, subs->left, type_long()));
        exp_set_type(exp, subs->right->type->ptr.ref);        
    } else {
        err_report(EC_ERROR, &exp->loc, "invalid types for `[]`.");
        exp_set_type(exp, type_int());
    }

    return exp;
}

//
// Type check an expression, and return the type checked expression (which may be
// the same object, or a new object).
//
static Expression* ast_check_expression(TypeCheckState *state, Expression *exp)
{
    switch (exp->tag) {
        case EXP_INT:           return ast_check_int(state, exp);
        case EXP_LONG:          return ast_check_long(state, exp);
        case EXP_UINT:          return ast_check_uint(state, exp);
        case EXP_ULONG:         return ast_check_ulong(state, exp);
        case EXP_FLOAT:         return ast_check_float(state, exp);
        case EXP_VAR:           return ast_check_var(state, exp);
        case EXP_UNARY:         return ast_check_unary(state, exp);
        case EXP_BINARY:        return ast_check_binary(state, exp);
        case EXP_CONDITIONAL:   return ast_check_conditional(state, exp);
        case EXP_ASSIGNMENT:    return ast_check_assignment(state, exp);
        case EXP_FUNCTION_CALL: return ast_check_function_call(state, exp);
        case EXP_CAST:          return ast_check_cast(state, exp);
        case EXP_DEREF:         return ast_check_deref(state, exp);
        case EXP_ADDROF:        return ast_check_addrof(state, exp);
        case EXP_SUBSCRIPT:     return ast_check_subscript(state, exp);
    }

    ICE_ASSERT(((void)"invalid expression tag in ast_check_expression", false));
    return NULL;
}

//
// Type check a return statement.
//
static void ast_check_return(TypeCheckState *state, StmtReturn *ret)
{
    exp_replace(&ret->exp, typecheck_and_convert(state, ret->exp));

    ret->exp = convert_by_assignment(state, ret->exp, type_clone(state->func_type->func.ret));
}

//
// Type check an if statement.
//
static void ast_check_if(TypeCheckState *state, StmtIf *ifelse)
{
    exp_replace(&ifelse->condition, typecheck_and_convert(state, ifelse->condition));
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
    exp_replace(&while_->cond, typecheck_and_convert(state, while_->cond));
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
        case FI_EXPRESSION:     exp_replace(&for_->init->exp, typecheck_and_convert(state, for_->init->exp)); break;
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
        exp_replace(&for_->cond, typecheck_and_convert(state, for_->cond));
    }

    if (for_->post) {
        exp_replace(&for_->post, typecheck_and_convert(state, for_->post));
    }

    ast_check_statement(state, for_->body);
}

//
// Type check a do while loop.
//
static void ast_check_do_while(TypeCheckState *state, StmtDoWhile *dowhile)
{
    exp_replace(&dowhile->cond, typecheck_and_convert(state, dowhile->cond));
    ast_check_statement(state, dowhile->body);
}

//
// Type check a switch statement.
//
static void ast_check_switch(TypeCheckState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_SWITCH);
    StmtSwitch *switch_ = &stmt->switch_;

    exp_replace(&switch_->cond, typecheck_and_convert(state, switch_->cond));

    if (!type_integral(switch_->cond->type)) {
        err_report(EC_ERROR, &stmt->loc, "switch conditional must be of integral type.");
    }

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
        case STMT_EXPRESSION:   exp_replace(&stmt->exp.exp, typecheck_and_convert(state, stmt->exp.exp)); break;
        case STMT_LABEL:        ast_check_label(state, &stmt->label); break;
        case STMT_GOTO:         break;
        case STMT_COMPOUND:     ast_check_compound(state, &stmt->compound); break;
        case STMT_WHILE:        ast_check_while(state, &stmt->while_); break;
        case STMT_FOR:          ast_check_for(state, &stmt->for_); break;
        case STMT_DOWHILE:      ast_check_do_while(state, &stmt->dowhile); break;
        case STMT_BREAK:        break;
        case STMT_CONTINUE:     break;
        case STMT_SWITCH:       ast_check_switch(state, stmt); break;
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
// Type check a function declaration parameter list. Types will be added to
// the symbol table for the parameters, and the declaration type will be 
// updated if needed.
//
static void ast_check_func_param_list(TypeCheckState *state, DeclFunction *func)
{
    ICE_ASSERT(func->type->tag == TT_FUNC);

    ListNode *pcurr = func->parms.head;
    ListNode *tcurr = func->type->func.parms.head;

    for (; pcurr && tcurr; pcurr = pcurr->next, tcurr = tcurr->next) {
        FuncParameter *parm = CONTAINER_OF(pcurr, FuncParameter, list);
        TypeFuncParam *ptype = CONTAINER_OF(tcurr, TypeFuncParam, list);

        if (ptype->parmtype->tag == TT_ARRAY) {
            ptype->parmtype = type_pointer(ptype->parmtype->ptr.ref);
        }

        Symbol *sym = stab_lookup(state->stab, parm->name);

        if (sym->type == NULL) {
            Type *symtype = type_clone(ptype->parmtype);
            sym_update_local(sym, symtype);
        }
    }

    ICE_ASSERT(pcurr == NULL && tcurr == NULL);
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

    if (func->func.type->func.ret->tag == TT_ARRAY) {
        err_report(EC_ERROR, &func->loc, "function may not return array type.");
    }

    ast_check_func_param_list(state, &func->func);

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

 
    if (func->func.has_body) {
        Type *old_func_type = state->func_type;
        state->func_type = sym->type;
        ast_check_block(state, func->func.body);
        state->func_type = old_func_type;
    }
}

//
// Given a an integer constant source, cast to a numeric target type.
//
static Const ast_cast_int_value_to_const(ExpressionTag src_tag, unsigned long src, Type *target)
{
    //
    // $TARGET all of this code assumes we are building native
    //
    if (target->tag == TT_DOUBLE) {
        switch (src_tag) {
            case EXP_INT:       return const_make_double((double)(int)src); break;
            case EXP_UINT:      return const_make_double((double)(unsigned)src); break;
            case EXP_LONG:      return const_make_double((double)(long)src); break;
            case EXP_ULONG:     return const_make_double((double)src); break;

            default:
                ICE_ASSERT(((void)"ast_cast_int_value_to_const: invalid source type tag", false));
                return const_make_double(0.0);
        }
    }

    unsigned long val = 0;

    //
    // integral -> integral
    //
    // first, make sure we have an unsigned long that is properly truncated/
    // extended based on the source type.
    //
    switch (src_tag) {
        case EXP_INT:       val = (int)src; break;
        case EXP_UINT:      val = (unsigned)src; break;
        case EXP_LONG:      val = (long)src; break;
        case EXP_ULONG:     val = src; break;

        default:
            ICE_ASSERT(((void)"ast_cast_int_value_to_const: invalid source type tag", false));
    }

    //
    // return a constant based on the requested target type.
    //
    switch (target->tag) {
        case TT_INT:    return const_make_int(CIS_INT,  CIS_SIGNED,   val);
        case TT_UINT:   return const_make_int(CIS_INT,  CIS_UNSIGNED, val);
        case TT_LONG:   return const_make_int(CIS_LONG, CIS_SIGNED,   val);
        case TT_ULONG:  return const_make_int(CIS_LONG, CIS_UNSIGNED, val);

        default:
            ICE_ASSERT(((void)"ast_cast_int_value_to_const: invalid target type tag", false));
    }

    return const_make_int(CIS_LONG, CIS_UNSIGNED, 0);
}

//
// Given a an floating point source, cast to a numeric target type.
//
static Const ast_cast_float_value_to_const(ExpressionTag src_tag, double src, Type *target)
{
    ICE_ASSERT(src_tag == EXP_FLOAT);


    switch (target->tag) {
        case TT_INT:    return const_make_int(CIS_INT,  CIS_SIGNED,   (unsigned long)(int)src);
        case TT_UINT:   return const_make_int(CIS_INT,  CIS_UNSIGNED, (unsigned long)(unsigned)src);
        case TT_LONG:   return const_make_int(CIS_LONG, CIS_SIGNED,   (unsigned long)(long)src);
        case TT_ULONG:  return const_make_int(CIS_LONG, CIS_UNSIGNED, (unsigned long)src);
        case TT_DOUBLE: return const_make_double(src);

        default:
            ICE_ASSERT(((void)"ast_cast_float_value_to_const: invalid target type tag", false));
    }

    return const_make_int(CIS_LONG, CIS_UNSIGNED, 0);
}


//
// Given a possibly nested initializer, return a flattened list of static initializers.
//
// Properly cast the initializers and put them in constants of the target type so code 
// generation know how to emit them without referring back to the variable type.
//
// Returns true if initializer was a scalar, else false.
//
static bool ast_flatten_compound_init_for_static(Initializer *init, Type *target, List *out, FileLine loc)
{
    if (init->tag == INIT_SINGLE) {
        Const cn = const_make_int(CIS_INT, CIS_SIGNED, 0);
        Expression *exp = init->single;

        if (!exp_is_constant(exp)) {
            err_report(EC_ERROR, &loc, "static initializer must be a constant.");
        } else if (!type_arithmetic(target) && target->tag != TT_POINTER) {
            err_report(EC_ERROR, &loc, "cannot initialize an aggregate with a scalar.");
        } else if (init->single->tag == EXP_FLOAT && target->tag == TT_POINTER) {
            err_report(EC_ERROR, &loc, "cannot initialize pointer with double.");
            cn = const_make_int(CIS_LONG, CIS_UNSIGNED, 0);
        } else if (target->tag == TT_POINTER) {
            //
            // Should be assured by checks above
            //
            ICE_ASSERT(exp_is_int_constant(exp));

            if (exp->intval) {
                err_report(EC_ERROR, &loc, "cannot initialize pointer with non-zero integer.");
            }

            //
            // $TARGET assuming pointer is sizeof(unsigned long)
            //
            cn = const_make_int(CIS_LONG, CIS_UNSIGNED, 0);
        } else if (exp_is_int_constant(exp)) {
            cn = ast_cast_int_value_to_const(exp->tag, exp->intval, target);
        } else {
            //
            // exp is a floating point constant, target is non-pointer
            //
            cn = ast_cast_float_value_to_const(exp->tag, exp->floatval, target);
        }

        StaticInitializer *si = sinit_make_const(cn);
        list_push_back(out, &si->list);

        return true;
    }

    ICE_ASSERT(target->tag == TT_ARRAY);        
    
    for (ListNode *curr = init->compound.head; curr; curr = curr->next) {
        Initializer *sub = CONTAINER_OF(curr, Initializer, list);
        ast_flatten_compound_init_for_static(sub, target->array.element, out, loc);
    }

    size_t init_count = list_count(&init->compound);
    size_t arr_count  = target->array.size;

    if (init_count > arr_count) {
        err_report(EC_ERROR, &loc, "too many initializers.");
    } else if (init_count < arr_count) {
        size_t padding = (arr_count - init_count) * type_size(target->array.element);
        StaticInitializer *si = sinit_make_zero(padding);
        list_push_back(out, &si->list);
    }

    return false;
}

//
// Type check a static initializer list against the declaration type.
//
// If the type is not an array, the list must have at most one element, which must be
// convertible to the declaration type.
//
// If the type is an array, then the list must not be longer than the number of the elements
// in the array, and each initializer must be convertible to the element tyoe.
//
// If the initializer is too short, it will be padded with a zero initializer.
//
static void ast_check_static_init(Type *type, List *init, bool init_scalar, FileLine loc)
{
    Type *base = type_array_element(type);
    int size = type_array_size(type);
    int init_size = list_count(init);

    if (size > init_size) {
        size_t init_bytes = type_size(base) * init_size;
        size_t decl_bytes = type_size(type);
        ICE_ASSERT(decl_bytes > init_bytes);
        StaticInitializer *zeroes = sinit_make_zero(decl_bytes - init_bytes);
        list_push_back(init, &zeroes->list);
    }
}

//
// Type check a file scope variable declaration.
//
static void ast_check_global_var_decl(TypeCheckState *state, Declaration *decl)
{
    ICE_ASSERT(decl->tag == DECL_VARIABLE);
    DeclVariable *var = &decl->var;

    StaticInitialValue siv = SIV_NO_INIT;
    bool init_scalar = false;

    List init;
    list_clear(&init);

    if (var->init == NULL) {
        if (var->storage_class == SC_EXTERN) {
            siv = SIV_NO_INIT;
        } else {
            siv = SIV_TENTATIVE;
        }
    } else {
        siv = SIV_INIT;

        list_clear(&init);
        init_scalar = ast_flatten_compound_init_for_static(var->init, decl->var.type, &init, decl->loc);
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

    ast_check_static_init(type, &init, init_scalar, decl->loc);

    sym_update_static_var(sym, type, siv, init, globally_visible, decl->loc);
}

//
// Create a scalar zero initializer.
//
static Initializer *make_zero_init_scalar(Type *type, Expression *exp)
{
    Initializer *init = init_single(exp);
    init->type = type_clone(type);
    return init;
}

//
// Create a zero initializer for an array.
//
static Initializer *make_zero_init_array(TypeCheckState *state, Type *type, FileLine loc)
{
    ICE_ASSERT(type->tag == TT_ARRAY);

    List inits;
    list_clear(&inits);

    for (int i = 0; i < type->array.size; i++) {
        Initializer *init = make_zero_init(state, type->array.element, loc);
        list_push_back(&inits, &init->list);
    }

    Initializer *init = init_compound(inits);
    init->type = type_clone(type);

    return init;
}

//
// Create an initialize of the shape of the given type.
//
static Initializer *make_zero_init(TypeCheckState *state, Type *type, FileLine loc)
{
    switch (type->tag) {
        case TT_ARRAY:      return make_zero_init_array(state, type, loc);
        case TT_INT:        return make_zero_init_scalar(type, exp_int(state->ast, 0, loc));
        case TT_UINT:       return make_zero_init_scalar(type, exp_uint(state->ast, 0, loc));
        case TT_LONG:       return make_zero_init_scalar(type, exp_long(state->ast, 0, loc));
        case TT_ULONG:      return make_zero_init_scalar(type, exp_ulong(state->ast, 0, loc));
        case TT_DOUBLE:     return make_zero_init_scalar(type, exp_float(state->ast, 0.0, loc));
        case TT_POINTER:    return make_zero_init_scalar(type, exp_ulong(state->ast, 0, loc));
        case TT_FUNC:       ICE_ASSERT(((void)"TT_FUNC type is invalid in make_zero_init", false));
    }

    return make_zero_init_scalar(type_int(), exp_int(state->ast, 0, loc));
}

//
// Check the initializer of a non-static variable.
//
static void ast_check_var_init(TypeCheckState *state, Type *target, Initializer *init, FileLine loc)
{
    if (init->tag == INIT_SINGLE) {
        Expression *exp = typecheck_and_convert(state, init->single);
        exp = convert_by_assignment(state, exp, target);
        exp_replace(&init->single, exp);
        init->type = type_clone(target);
        
        return;
    }

    //
    // Compound initializer
    //
    if (target->tag != TT_ARRAY) {
        err_report(EC_ERROR, &loc, "cannot initialize scalar with compound initializer.");
        return;
    }

    int init_count = list_count(&init->compound);
    if (init_count > target->array.size) {
        err_report(EC_ERROR, &loc, "too many initializers for array.");
        return;
    }

    for (ListNode *curr = init->compound.head; curr; curr = curr->next) {
        Initializer *subinit = CONTAINER_OF(curr, Initializer, list);
        ast_check_var_init(state, target->array.element, subinit, loc);
    }

    for (int i = 0; i < target->array.size - init_count; i++) {
        Initializer *zero = make_zero_init(state, target->array.element, loc);
        list_push_back(&init->compound, &zero->list);
    }
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
    bool init_scalar = false;

    Symbol *sym = stab_lookup(state->stab, var->name);
    
    if (sym->type && !types_equal(sym->type, var->type)) {
        err_report(EC_ERROR, &decl->loc, "cannot redeclare variable `%s` as different type.", var->name);
    }

    Type *type = type_clone(var->type);

    List init;
    list_clear(&init);

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
            ast_check_static_init(type, &init, init_scalar, decl->loc);
            sym_update_static_var(sym, type, SIV_NO_INIT, init, true, decl->loc);
        }
    } else if (var->storage_class == SC_STATIC) {
        if (var->init == NULL) {
            ast_check_static_init(type, &init, init_scalar, decl->loc);
            sym_update_static_var(sym, type, SIV_INIT, init, false, decl->loc);
        } else {
            init_scalar = ast_flatten_compound_init_for_static(var->init, var->type, &init, decl->loc);
            ast_check_static_init(type, &init, init_scalar, decl->loc);
            sym_update_static_var(sym, type, SIV_INIT, init, false, decl->loc);
        }
    } else {
        //
        // No storage class
        //
        sym_update_local(sym, type);
        if (var->init) {
            ast_check_var_init(state, type, var->init, decl->loc);
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
void ast_typecheck(AstProgram *prog, SymbolTable *stab, AstState *ast)
{
    TypeCheckState state;

    state.stab = stab;
    state.ast = ast;

    for (ListNode *curr = prog->decls.head; curr; curr = curr->next) {
        Declaration *decl = CONTAINER_OF(curr, Declaration, list);
        ast_check_declaration(&state, decl, true);
    } 
}
