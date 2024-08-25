#include "tacgen.h"

#include "constant.h"
#include "errors.h"
#include "hashtab.h"
#include "ice.h"
#include "list.h"
#include "safemem.h"
#include "symtab.h"
#include "tacnode.h"
#include "temporary.h"
#include "typetab.h"

typedef struct {
    List code;              // list of TacNode *
    SymbolTable *stab;      // symbol table
    TypeTable *typetab;     // type table
} TacState;

typedef enum {
    ER_PLAIN,               // just a regular operand
    ER_DEREF_POINTER,       // a dereferenced pointer
} TacExpResultTag;

typedef struct {
    TacExpResultTag tag;

    union {
        TacNode *plain;
        TacNode *deref_pointer;
    };
} TacExpResult;

static TacExpResult tcg_expression(TacState *state, Expression *exp);
static void tcg_statement(TacState *state, Statement *stmt);
static void tcg_append(TacState *state, TacNode *tac);
static TacNode *tcg_temporary(TacState *state, Type *type, FileLine loc);

//
// NOTE TAC generation expects a syntactically correct program from
//      the parser; hence, we use ICE_ASSERT to check most errors.
//

//
// Construct an expression result for a plain operand.
//
static TacExpResult expres_plain(TacNode *node)
{
    TacExpResult expres;
    expres.tag = ER_PLAIN;
    expres.plain = node;
    return expres;
}

//
// Construct an expression result for a dereferenced pointer operand.
//
static TacExpResult expres_deref(TacNode *deref)
{
    TacExpResult expres;
    expres.tag = ER_DEREF_POINTER;
    expres.deref_pointer = deref;
    return expres;
}

//
// Generate TAC to dereference a pointer and return the result.
//
static TacNode *tcg_deref_pointer(TacState *state, TacNode *ptr, Type *type)
{
    TacNode *value = tcg_temporary(state, type_clone(type), ptr->loc);
    tcg_append(state, tac_load(ptr, value, ptr->loc));
    return tac_clone_operand(value);
}

//
// Convert an expression to TAC and l-value convert the result.
//
static TacNode *tcg_expression_and_convert(TacState *state, Expression *exp)
{
    TacExpResult result = tcg_expression(state, exp);

    switch (result.tag) {
        case ER_PLAIN:          return result.plain;
        case ER_DEREF_POINTER:  return tcg_deref_pointer(state, result.deref_pointer, exp->type);
    }

    ICE_ASSERT(((void)"invalid tag in tcg_expression_and_convert", false));
    return NULL;

} 

//
// Format a loop labvel with a unique tag. Returns an 
// allocated string.
//
static char *tcg_loop_tag_label(char *tag, int label)
{
    ICE_ASSERT(label >= 0);

    char *base = saprintf("loop_%s", tag);
    char *name = tmp_id_name(base, label);
    safe_free(base);
    return name;
}

//
// Format a continue loop label. Returns an allocated string.
//
static char *tcg_continue_label(int label)
{
    return tcg_loop_tag_label("continue", label);
}

//
// Format a break loop label. Returns an allocated string.
//
static char *tcg_break_label(int label)
{
    return tcg_loop_tag_label("break", label);
}

//
// Format a label for a case statement. Returns an allocated string.
//
static char *tcg_case_label(int label, unsigned long value)
{
    char *tag = saprintf("case_%lx", value);
    char *looplabel = tcg_loop_tag_label(tag, label);
    safe_free(tag);
    return looplabel;
}

//
// Format a label for a default statement. Returns an allocated string.
//
static char *tcg_default_label(int label)
{
    return tcg_loop_tag_label("default", label);
}

//
// Append a TAC instruction to the code list.
//
static void tcg_append(TacState *state, TacNode *tac)
{
    list_push_back(&state->code, &tac->list);
}

//
// Return a unique temporary variable.
//
static TacNode *tcg_temporary(TacState *state, Type *type, FileLine loc)
{
    char *name = tmp_name("tmp");

    TacNode *var = tac_var(name, loc);

    Symbol *sym = stab_lookup(state->stab, name);
    sym_update_local(sym, type);

    safe_free(name);

    return var;
}

//
// Return a globally unique label.
//
static TacNode *tcg_make_label(FileLine loc)
{
    char *name = tmp_name("label");
    TacNode *label = tac_label(name, loc);
    safe_free(name);
    return label;
}

//
// Return TAC for an increment or decrement unary operator.
//
static TacExpResult tcg_unary_incdec(TacState *state, Expression *unary)
{
    FileLine loc = unary->loc;
    BinaryOp bop = BOP_ADD;

    switch (unary->unary.op) {
        case UOP_PREDECREMENT:
        case UOP_POSTDECREMENT:     bop = BOP_SUBTRACT; break;

        case UOP_PREINCREMENT:
        case UOP_POSTINCREMENT:     bop = BOP_ADD; break;

        default:
            ICE_ASSERT(((void)"invalid operator in tcg_unary_indec", false));
    }

    Const one = const_make_int(CIS_INT, CIS_SIGNED, 1);
    if (unary->unary.exp->type->tag == TT_DOUBLE) {
        one = const_make_double(1);
    }

    if (unary->unary.op == UOP_PREDECREMENT || unary->unary.op == UOP_PREINCREMENT) {
        TacNode *post_value = tcg_temporary(state, type_clone(unary->type), loc);
        TacExpResult operand_res = tcg_expression(state, unary->unary.exp);

        TacNode *operand = NULL;

        switch (operand_res.tag) {
            case ER_PLAIN:          
                operand = operand_res.plain; 
                break;

            case ER_DEREF_POINTER:
                operand = tcg_deref_pointer(state, operand_res.deref_pointer, type_clone(unary->unary.exp->type));
                break;
        }

        tcg_append(state, tac_binary(bop, operand, tac_const(one, loc), post_value, loc));

        switch (operand_res.tag) {
            case ER_PLAIN:
                tcg_append(state, tac_copy(tac_clone_operand(post_value), tac_clone_operand(operand), loc));
                break;

            case ER_DEREF_POINTER:
                tcg_append(state, tac_store(tac_clone_operand(post_value), tac_clone_operand(operand_res.deref_pointer), loc));
                break;
        }

        return expres_plain(tac_clone_operand(post_value));
    }

    TacNode *old_value = tcg_temporary(state, type_clone(unary->type), loc);
    TacNode *post_value = tcg_temporary(state, type_clone(unary->type), loc);
    TacExpResult operand_res = tcg_expression(state, unary->unary.exp);

    TacNode *operand = NULL;

    switch (operand_res.tag) {
        case ER_PLAIN:
            operand = operand_res.plain;
            break;

        case ER_DEREF_POINTER:
            operand = tcg_deref_pointer(state, operand_res.deref_pointer, type_clone(unary->unary.exp->type));
            break;
    }
            
    tcg_append(state, tac_copy(tac_clone_operand(operand), old_value, loc));

    tcg_append(state, tac_binary(bop, operand, tac_const(one, loc), post_value, loc));

    switch (operand_res.tag) {
        case ER_PLAIN:
            tcg_append(state, tac_copy(tac_clone_operand(post_value), tac_clone_operand(operand), loc));
            break;

        case ER_DEREF_POINTER:
            tcg_append(state, tac_store(tac_clone_operand(post_value), tac_clone_operand(operand_res.deref_pointer), loc));
            break;
    }

    return expres_plain(tac_clone_operand(old_value));
}

//
// Return TAC for a unary operation.
//
static TacExpResult tcg_unary_op(TacState *state, Expression *unary)
{
    if (
        unary->unary.op == UOP_PREDECREMENT ||
        unary->unary.op == UOP_PREINCREMENT ||
        unary->unary.op == UOP_POSTDECREMENT ||
        unary->unary.op == UOP_POSTINCREMENT) {
        
        return tcg_unary_incdec(state, unary);
    }

    FileLine loc = unary->loc;
    TacNode *src = tcg_expression_and_convert(state, unary->unary.exp);
    TacNode *dst = tcg_temporary(state, type_clone(unary->type), loc);

    tcg_append(state, tac_unary(unary->unary.op, src, dst, unary->loc));

    return expres_plain(tac_var(dst->var.name, unary->loc));
}

//
// Return TAC for a short-circuit logical operator; i.e. && or ||
//
static TacExpResult tcg_short_circuit_op(TacState *state, Expression *binary)
{
    ICE_ASSERT(binary->tag == EXP_BINARY);
    ICE_ASSERT(binary->binary.op == BOP_LOGAND || binary->binary.op == BOP_LOGOR);

    FileLine loc = binary->loc;
    
    bool is_and = binary->binary.op == BOP_LOGAND;
    Const success_val = const_make_int(CIS_INT, CIS_SIGNED, is_and ? 1 : 0);
    Const fail_val = const_make_int(CIS_INT, CIS_SIGNED, is_and ? 0 : 1);

    TacNode *(*tac_sc_jump)(TacNode *, char *, FileLine) = is_and ? tac_jump_zero : tac_jump_not_zero;

    TacNode *tf_label = tcg_make_label(loc);
    TacNode *end_label = tcg_make_label(loc);
    TacNode *result = tcg_temporary(state, type_clone(binary->type), loc);
    
    TacNode *left = tcg_expression_and_convert(state, binary->binary.left);
    tcg_append(state, tac_sc_jump(left, tf_label->label.name, loc));
    TacNode *right = tcg_expression_and_convert(state, binary->binary.right);
    tcg_append(state, tac_sc_jump(right, tf_label->label.name, loc));
    tcg_append(state, tac_copy(tac_const(success_val, loc), tac_var(result->var.name, loc), loc));
    tcg_append(state, tac_jump(end_label->label.name, loc));
    tcg_append(state, tf_label);
    tcg_append(state, tac_copy(tac_const(fail_val, loc), tac_var(result->var.name, loc), loc));
    tcg_append(state, end_label);

    return expres_plain(result);
}

//
// Generate TAC for pointer addition.
// 
static TacExpResult tcg_pointer_add(TacState *state, Expression *binary)
{
    //
    // From typecheck, we know that exacly one of the operands is a pointer.
    //
    bool lptr = binary->binary.left->type->tag == TT_POINTER;
    bool rptr = binary->binary.right->type->tag == TT_POINTER;

    ICE_ASSERT(lptr ^ rptr);

    Expression *eptr = lptr ? binary->binary.left : binary->binary.right;
    Expression *eidx = rptr ? binary->binary.left : binary->binary.right;

    size_t element_size = type_size(state->typetab, eptr->type->ptr.ref);

    TacNode *ptr = tcg_expression_and_convert(state, eptr);
    TacNode *idx = tcg_expression_and_convert(state, eidx);
    TacNode *dst = tcg_temporary(state, type_clone(binary->type), binary->loc);

    tcg_append(state, tac_add_ptr(ptr, idx, element_size, dst, binary->loc));

    return expres_plain(tac_var(dst->var.name, binary->loc));
}

//
// Generate TAC for pointer subtraction (either ptr - ptr, or ptr - integral).
//
static TacExpResult tcg_pointer_sub(TacState *state, Expression *binary)
{
    size_t element_size = type_size(state->typetab, binary->binary.left->type->ptr.ref);

    if (binary->binary.right->type->tag == TT_POINTER)
    {
        //
        // ptr - ptr
        //
        TacNode *left = tcg_expression_and_convert(state, binary->binary.left);
        TacNode *right = tcg_expression_and_convert(state, binary->binary.right);
        TacNode *diff = tcg_temporary(state, type_long(), binary->loc);
        TacNode *result = tcg_temporary(state, type_long(), binary->loc);

        Const scale;
        scale = const_make_int(CIS_LONG, CIS_UNSIGNED, element_size);

        tcg_append(state, tac_binary(BOP_SUBTRACT, left, right, diff, binary->loc));
        tcg_append(state, tac_binary(BOP_DIVIDE, tac_var(diff->var.name, binary->loc), tac_const(scale, binary->loc), result, binary->loc));

        return expres_plain(tac_var(result->var.name, binary->loc));
    }
    else
    {
        //
        // ptr - integral
        //
        TacNode *ptr = tcg_expression_and_convert(state, binary->binary.left);
        TacNode *idx = tcg_expression_and_convert(state, binary->binary.right);

        TacNode *neg = tcg_temporary(state, type_clone(binary->binary.right->type), binary->loc);
        TacNode *dst = tcg_temporary(state, type_clone(binary->binary.left->type), binary->loc);
    
        tcg_append(state, tac_unary(UOP_MINUS, idx, neg, binary->loc));

        tcg_append(state, tac_add_ptr(ptr, tac_var(neg->var.name, binary->loc), element_size, dst, binary->loc));
        return expres_plain(tac_var(dst->var.name, binary->loc));
    }
}

//
// Generate TAC for a binary expression.
// 
static TacExpResult tcg_binary_op(TacState *state, Expression *binary)
{
    ICE_ASSERT(binary->tag == EXP_BINARY);

    //
    // Special handling for operators which require short-circuit evaluation
    //
    if (binary->binary.op == BOP_LOGAND || binary->binary.op == BOP_LOGOR) {
        return tcg_short_circuit_op(state, binary);
    }

    //
    // Some operators have special cases for pointers.
    //
    bool pointers = binary->binary.left->type->tag == TT_POINTER || binary->binary.right->type->tag == TT_POINTER;

    if (pointers) {
        switch (binary->binary.op) {
            case BOP_ADD:       return tcg_pointer_add(state, binary);
            case BOP_SUBTRACT:  return tcg_pointer_sub(state, binary);

            default: break;
        }
    }
    
    TacNode *left = tcg_expression_and_convert(state, binary->binary.left);
    TacNode *right = tcg_expression_and_convert(state, binary->binary.right);
    TacNode *dst = tcg_temporary(state, type_clone(binary->type), binary->loc);


    tcg_append(state, tac_binary(binary->binary.op, left, right, dst, binary->loc));

    return expres_plain(tac_var(dst->var.name, binary->loc));
}

//
// Generate TAC for a conditional expression.
//
static TacExpResult tcg_conditional(TacState *state, Expression *condexp)
{
    ICE_ASSERT(condexp->tag == EXP_CONDITIONAL);
    ExpConditional *cond = &condexp->conditional;

    bool isvoid = condexp->type->tag == TT_VOID;

    TacNode *falsepart = tcg_make_label(cond->falseval->loc);
    TacNode *end = tcg_make_label(cond->falseval->loc);
    TacNode *result = 
        isvoid ? tac_var("dummy", cond->cond->loc) :
        tcg_temporary(state, type_clone(condexp->type), cond->cond->loc);

    TacNode *condval = tcg_expression_and_convert(state, cond->cond);
    tcg_append(state, tac_jump_zero(condval, falsepart->label.name, cond->cond->loc));
    TacNode *trueval = tcg_expression_and_convert(state, cond->trueval);
    if (!isvoid) {
        tcg_append(state, tac_copy(trueval, tac_clone_operand(result), cond->trueval->loc));
    } else {
        tac_free(trueval);
    }
    tcg_append(state, tac_jump(end->label.name, cond->trueval->loc));
    tcg_append(state, tac_label(falsepart->label.name, cond->falseval->loc));
    TacNode *falseval = tcg_expression_and_convert(state, cond->falseval);
    if (!isvoid) {
       tcg_append(state, tac_copy(falseval, tac_clone_operand(result), cond->falseval->loc));
    } else {
        tac_free(falseval);
    }
    tcg_append(state, tac_label(end->label.name, cond->falseval->loc));

    return expres_plain(result);
}

//
// Generate TAC for an assignment.
//
static TacExpResult tcg_assignment(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_ASSIGNMENT);
    ExpAssignment *assign = &exp->assignment;

    TacExpResult left = tcg_expression(state, assign->left);
    TacNode *right = tcg_expression_and_convert(state, assign->right);
    TacNode *retval;

    if (assign->op == BOP_ASSIGN) {
        switch (left.tag) {
            case ER_PLAIN: 
                tcg_append(state, tac_copy(right, left.plain, exp->loc)); 
                retval = left.plain;
                break;

            case ER_DEREF_POINTER:
                tcg_append(state, tac_store(right, left.deref_pointer, exp->loc));
                retval = right;
                break;
        }

    } else {
        ICE_ASSERT(bop_is_compound_assign(assign->op));
        BinaryOp bop = bop_compound_to_binop(assign->op);
        TacNode *tmp = tcg_temporary(state, type_clone(exp->type), exp->loc);
        TacNode *tmp2;

        switch (left.tag) {
            case ER_PLAIN:
                tcg_append(state, tac_binary(bop, left.plain, right, tmp, exp->loc));
                tcg_append(state, tac_copy(tac_clone_operand(tmp), tac_clone_operand(left.plain), exp->loc));
                retval = left.plain;
                break;

            case ER_DEREF_POINTER:
                tmp2 = tcg_temporary(state, type_clone(exp->type), exp->loc);
                tcg_append(state, tac_load(left.deref_pointer, tmp2, exp->loc));
                tcg_append(state, tac_binary(bop, tmp2, right, tmp, exp->loc));
                tcg_append(state, tac_store(tmp, left.deref_pointer, exp->loc));
                retval = tmp;
                break;
        }
    }

    return expres_plain(tac_clone_operand(retval));
}

//
// Generate TAC for a function call.
//
static TacExpResult tcg_function_call(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_FUNCTION_CALL);
    ExpFunctionCall *call = &exp->call;

    List args;
    list_clear(&args);

    for (ListNode *curr = call->args.head; curr; curr = curr->next) {
        Expression *arg = CONTAINER_OF(curr, Expression, list);
        TacNode *tacarg = tcg_expression_and_convert(state, arg);
        list_push_back(&args, &tacarg->list);
    }

    TacNode *result = tcg_temporary(state, type_clone(exp->type), exp->loc);

    tcg_append(state, tac_function_call(call->name, args, result, exp->loc));

    return expres_plain(tac_clone_operand(result));
}

//
// Generate TAC for a constant signed char.
//
static TacExpResult tcg_const_schar(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_SCHAR);

    Const cn = const_make_int(CIS_CHAR, CIS_SIGNED, exp->intval);
    return expres_plain(tac_const(cn, exp->loc));
}

//
// Generate TAC for a constant unsigned char.
//
static TacExpResult tcg_const_uchar(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_UCHAR);

    Const cn = const_make_int(CIS_CHAR, CIS_UNSIGNED, exp->intval);
    return expres_plain(tac_const(cn, exp->loc));
}

//
// Generate TAC for a constant integer.
//
static TacExpResult tcg_const_int(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_INT);

    Const cn = const_make_int(CIS_INT, CIS_SIGNED, exp->intval);
    return expres_plain(tac_const(cn, exp->loc));
}

//
// Generate TAC for a constant long.
//
static TacExpResult tcg_const_long(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_LONG);

    Const cn = const_make_int(CIS_LONG, CIS_SIGNED, exp->intval); 
    return expres_plain(tac_const(cn, exp->loc));
}

//
// Generate TAC for a constant unsigned integer.
//
static TacExpResult tcg_const_uint(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_UINT);

    Const cn = const_make_int(CIS_INT, CIS_UNSIGNED, exp->intval);
    return expres_plain(tac_const(cn, exp->loc));
}

//
// Generate TAC for a constant long.
//
static TacExpResult tcg_const_ulong(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_ULONG);

    Const cn = const_make_int(CIS_LONG, CIS_UNSIGNED, exp->intval);
    return expres_plain(tac_const(cn, exp->loc));
}

//
// Generate TAC for a constant float.
//
static TacExpResult tcg_const_float(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_FLOAT);

    Const cn = const_make_double(exp->floatval);
    return expres_plain(tac_const(cn, exp->loc));
}

//
// Generate TAC for cast.
//
static TacExpResult tcg_cast(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_CAST);
    ExpCast *cast = &exp->cast;

    TacNode *inner = tcg_expression_and_convert(state, cast->exp);

    if (cast->type->tag == TT_VOID) {
        return expres_plain(tac_var("dummy", exp->loc));
    }

    if (types_equal(cast->exp->type, cast->type)) {
        return expres_plain(inner);
    }

    TacNode *tmp = tcg_temporary(state, type_clone(cast->type), exp->loc);

    if (cast->type->tag == TT_DOUBLE) {
        if (type_unsigned(cast->exp->type)) {
            tcg_append(state, tac_uint_to_double(inner, tmp, exp->loc));
        } else {
            tcg_append(state, tac_int_to_double(inner, tmp, exp->loc));
        }
    } else if (cast->exp->type->tag == TT_DOUBLE) {
        if (type_unsigned(cast->type)) {
            tcg_append(state, tac_double_to_uint(inner, tmp, exp->loc));
        } else {
            tcg_append(state, tac_double_to_int(inner, tmp, exp->loc));
        }
    } else if (types_same_size(state->typetab, cast->exp->type, cast->type)) {
        tcg_append(state, tac_copy(inner, tmp, exp->loc));
    } else if (type_size(state->typetab, cast->type) < type_size(state->typetab, cast->exp->type)) {
        tcg_append(state, tac_truncate(inner, tmp, exp->loc));
    } else if (type_unsigned(cast->exp->type)) {
        tcg_append(state, tac_zero_extend(inner, tmp, exp->loc));
    } else {
        tcg_append(state, tac_sign_extend(inner, tmp, exp->loc));
    }

    return expres_plain(tac_clone_operand(tmp));
}

//
// Generate TAC for a dereference.
//
static TacExpResult tcg_deref(TacState *state, Expression *exp)
{
    TacNode *ptr = tcg_expression_and_convert(state, exp->deref.exp);
    return expres_deref(ptr);
}

//
// Generate TAC for taking the address of an expression.
//
static TacExpResult tcg_addrof(TacState *state, Expression *exp)
{
    TacExpResult value = tcg_expression(state, exp->addrof.exp);
    TacNode *dst;

    switch (value.tag) {
        case ER_PLAIN:
            dst = tcg_temporary(state, type_clone(exp->type), exp->loc);
            tcg_append(state, tac_get_address(value.plain, dst, exp->loc));
            return expres_plain(tac_clone_operand(dst));

        case ER_DEREF_POINTER:
            //
            // Expression was something like &*ptr, which is by defintion ptr.
            //
            return expres_plain(value.deref_pointer);
    }

    ICE_ASSERT(((void)"invalid tag in tcg_addrof", false));
    return expres_plain(NULL);
}

//
// Generate TAC for a subscript operator.
//
// This is like pointer addition, but we return that the result should be 
// a dereferenced pointer.
//
static TacExpResult tcg_subscript(TacState *state, Expression *exp)
{
    //
    // From typecheck, we know that exacly one of the operands is a pointer.
    //
    bool lptr = exp->subscript.left->type->tag == TT_POINTER;
    bool rptr = exp->subscript.right->type->tag == TT_POINTER;

    ICE_ASSERT(lptr ^ rptr);

    Expression *eptr = lptr ? exp->subscript.left : exp->subscript.right;
    Expression *eidx = rptr ? exp->subscript.left : exp->subscript.right;

    size_t element_size = type_size(state->typetab, eptr->type->ptr.ref);

    TacNode *ptr = tcg_expression_and_convert(state, eptr);
    TacNode *idx = tcg_expression_and_convert(state, eidx);
    TacNode *dst = tcg_temporary(state, type_clone(eptr->type), exp->loc);

    tcg_append(state, tac_add_ptr(ptr, idx, element_size, dst, exp->loc));

    return expres_deref(tac_var(dst->var.name, exp->loc));
}

//
// Generate TAC for a string expression/
//
static TacExpResult tcg_const_string(TacState *state, Expression *exp)
{
    char *var = tmp_name("strlit");

    Type *type = type_array(type_char(), exp->string.length);
    Symbol *sym = stab_lookup(state->stab, var);
    ExpString *str = &exp->string;

    ICE_ASSERT(str->length);
    bool nul_terminated = str->data[str->length - 1] == '\0';

    StaticInitializer *si = sinit_make_string(exp->string.data, exp->string.length, nul_terminated);
    
    sym->tag = ST_CONSTANT;
    sym->type = type;
    sym->stconst = si;

    return expres_plain(tac_var(var, exp->loc));
}

//
// Generate TAC for sizeof operators.
//
static TacExpResult tcg_sizeof(TacState *state, Expression *exp)
{
    ExpSizeof *szof = &exp->sizeof_;

    size_t size = 0;

    switch (szof->tag) {
        case SIZEOF_EXP:
            size = type_size(state->typetab, szof->exp->type);
            break;

        case SIZEOF_TYPE:
            size = type_size(state->typetab, szof->type);
            break;
    }

    //
    // $TARGET assuming size_t is unsigned long
    //
    return expres_plain(tac_const(const_make_int(CIS_LONG, CIS_UNSIGNED, size), exp->loc));

}

//
// Generate TAC for an expression.
//
static TacExpResult tcg_expression(TacState *state, Expression *exp)
{
    switch (exp->tag) {
        case EXP_SCHAR:         return tcg_const_schar(state, exp);
        case EXP_UCHAR:         return tcg_const_uchar(state, exp);
        case EXP_INT:           return tcg_const_int(state, exp);
        case EXP_LONG:          return tcg_const_long(state, exp);
        case EXP_UINT:          return tcg_const_uint(state, exp);
        case EXP_ULONG:         return tcg_const_ulong(state, exp);
        case EXP_FLOAT:         return tcg_const_float(state, exp);
        case EXP_STRING:        return tcg_const_string(state, exp);
        case EXP_VAR:           return expres_plain(tac_var(exp->var.name, exp->loc));
        case EXP_UNARY:         return tcg_unary_op(state, exp);
        case EXP_BINARY:        return tcg_binary_op(state, exp);
        case EXP_CONDITIONAL:   return tcg_conditional(state, exp);
        case EXP_ASSIGNMENT:    return tcg_assignment(state, exp);
        case EXP_FUNCTION_CALL: return tcg_function_call(state, exp); break; 
        case EXP_CAST:          return tcg_cast(state, exp); break;
        case EXP_DEREF:         return tcg_deref(state, exp); break;
        case EXP_ADDROF:        return tcg_addrof(state, exp); break;
        case EXP_SUBSCRIPT:     return tcg_subscript(state, exp); break;
        case EXP_SIZEOF:        return tcg_sizeof(state, exp); break;
        case EXP_DOT:           ICE_NYI("tcg_expression::dot");
        case EXP_ARROW:         ICE_NYI("tcg_expression::arrow");
    }

    ICE_ASSERT(((void)"invalid expression node", false));
    
    //
    // never reached
    //
    return expres_plain(NULL);
}

//
// Generate TAC for initializing an array of char with a string literal.
//
// Optimize by taking the string 8 bytes at a time and using a 64-bit move.
//
static int tcg_nested_init_array_with_string(TacState *state, Type *type, ExpString *str, char *var, int offset, FileLine loc)
{
    ICE_ASSERT(type->tag == TT_ARRAY);

    int left = type->array.size;
    int src = 0;

    while (left > 8) {
        unsigned long val = 0;

        for (int i = 7; i >= 0; i--) {
            int index = src + i;
            unsigned char ch = index < str->length ? str->data[index] : 0;
            val = (val << 8) | ch;
        }

        Const cn = const_make_int(CIS_LONG, CIS_UNSIGNED, val);

        tcg_append(state, tac_copy_to_offset(tac_const(cn, loc), var, offset, loc));

        offset += 8;
        src += 8;
        left -= 8;
    }

    if (left > 4) {
        unsigned long val = 0;

        for (int i = 3; i >= 0; i--) {
            int index = src + i;
            unsigned char ch = index < str->length ? str->data[index] : 0;
            val = (val << 8) | ch;
        }

        Const cn = const_make_int(CIS_INT, CIS_UNSIGNED, val);

        tcg_append(state, tac_copy_to_offset(tac_const(cn, loc), var, offset, loc));

        offset += 4;
        src += 4;
        left -= 4;
    }
    
    while (left--) {
        unsigned char ch = src < str->length ? str->data[src] : 0;
        Const cn = const_make_int(CIS_CHAR, CIS_UNSIGNED, ch);

        tcg_append(state, tac_copy_to_offset(tac_const(cn, loc), var, offset, loc));

        src++;
        offset++;
    }

    return offset;
}

//
// Generate TAC for a nested initializer.
//
static void tcg_nested_init(TacState *state, Initializer *init, int *offset, Type *type, char *var, FileLine loc)
{
    Type *inner;

    if (init->tag == INIT_COMPOUND) {
        switch (type->tag) {
            case TT_ARRAY:      inner = type->array.element; break;
            case TT_STRUCT:     ICE_NYI("tcg_nested_init::struct");

            case TT_CHAR:
            case TT_UCHAR:
            case TT_SCHAR:
            case TT_INT:
            case TT_LONG:
            case TT_UINT:
            case TT_ULONG:
            case TT_DOUBLE:
            case TT_FUNC:
            case TT_VOID:
            case TT_POINTER:    ICE_ASSERT(((void)"invalid type tag for compound initializer in tcg_nested_init", false));
        }

        for (ListNode *curr = init->compound.head; curr; curr = curr->next) {
            Initializer *subinit = CONTAINER_OF(curr, Initializer, list);
            tcg_nested_init(state, subinit, offset, inner, var, loc);
        }

        return;
    }

    //
    // Single initializer
    //

    //
    // Special case: array initialized by string.
    //
    if (type->tag == TT_ARRAY && init->single->tag == EXP_STRING) {
        ICE_ASSERT(type_is_char(type->array.element));
            
        *offset = tcg_nested_init_array_with_string(state, type, &init->single->string, var, *offset, loc);
        return;
    }

    TacNode *val = tcg_expression_and_convert(state, init->single);
    tcg_append(state, tac_copy_to_offset(val, var, *offset, loc));
    *offset += type_size(state->typetab, type);
}

//
// Generate TAC for a declaration.
//
// The declaration itself was taken care of in the previous pass.
// However, if there is an initializer, we need to generate code for
// that here.
//
static void tcg_declaration(TacState *state, Declaration *decl)
{
    if (decl->tag != DECL_VARIABLE) {
        return;
    }

    DeclVariable *var = &decl->var;

    if (var->storage_class == SC_EXTERN || var->storage_class == SC_STATIC) {
        //
        // These will be taken care of later by enumerating the symbol table.
        //
        return;
    }

    //
    // For local variables, generate code for initializers
    //
    if (decl->var.init) {
        int offset = 0;
        tcg_nested_init(state, decl->var.init, &offset, decl->var.type, decl->var.name, decl->loc);
    }
}

//
// Generate TAC for a return statement.
//
static void tcg_return(TacState *state, Statement *ret)
{
    ICE_ASSERT(ret);
    ICE_ASSERT(ret->tag == STMT_RETURN);

    FileLine loc = ret->loc;

    TacNode *exp = NULL;
    
    if (ret->ret.exp) {
        exp = tcg_expression_and_convert(state, ret->ret.exp);
    }

    tcg_append(state, tac_return(exp, loc));
}

//g
// Generate code for an if statement
//
static void tcg_if(TacState *state, Statement *ifstmt)
{
    ICE_ASSERT(ifstmt->tag == STMT_IF);

    bool has_else = ifstmt->ifelse.elsepart != NULL;

    TacNode *falselabel = NULL;
    TacNode *endlabel = NULL;
    TacNode *condtarget = NULL;

    if (has_else) {
        falselabel = tcg_make_label(ifstmt->ifelse.elsepart->loc);
        endlabel = tcg_make_label(ifstmt->ifelse.elsepart->loc);
        condtarget = falselabel;
    } else {
        endlabel = tcg_make_label(ifstmt->loc);
        condtarget = endlabel;
    }

    TacNode *cond = tcg_expression_and_convert(state, ifstmt->ifelse.condition);
    tcg_append(state, tac_jump_zero(cond, condtarget->label.name, ifstmt->loc));
    tcg_statement(state, ifstmt->ifelse.thenpart);
    if (has_else) {
        tcg_append(state, tac_jump(endlabel->label.name, ifstmt->loc));
        tcg_append(state, falselabel);
        tcg_statement(state, ifstmt->ifelse.elsepart);
    }
    tcg_append(state, endlabel);
}

//
// Generate TAC for a program-defined label.
//
static void tcg_label(TacState *state, Statement *label)
{
    tcg_append(state, tac_label(label->label.name, label->loc));
    tcg_statement(state, label->label.stmt);
}

//
// Generate TAC for a goto statement
//
static void tcg_goto(TacState *state, Statement *goto_)
{
    tcg_append(state, tac_jump(goto_->goto_.target, goto_->loc));
}

//
// Generate TAC for a block. 
//
static void tcg_block(TacState *state, List items)
{
    for (ListNode *curr = items.head; curr; curr = curr->next) {
        BlockItem *blki = CONTAINER_OF(curr, BlockItem, list);
        
        switch (blki->tag) {
            case BI_DECLARATION:    tcg_declaration(state, blki->decl); break;
            case BI_STATEMENT:      tcg_statement(state, blki->stmt); break;
        }
    }
}

//
// Generate TAC for a compound statement.
//
static void tcg_compound(TacState *state, Statement *compound)
{
    tcg_block(state, compound->compound.items);
}

//
// Generate TAC for a while loop.
//
static void tcg_while(TacState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_WHILE);
    StmtWhile *while_ = &stmt->while_;

    char *cont = tcg_continue_label(while_->label);
    char *brk = tcg_break_label(while_->label);

    tcg_append(state, tac_label(cont, stmt->loc));
    TacNode *cond = tcg_expression_and_convert(state, while_->cond);
    tcg_append(state, tac_jump_zero(cond, brk, stmt->loc));
    tcg_statement(state, while_->body);
    tcg_append(state, tac_jump(cont, stmt->loc));
    tcg_append(state, tac_label(brk, stmt->loc));

    safe_free(cont);
    safe_free(brk);
}

//
// Generate TAC for a do while loop.
//
static void tcg_do_while(TacState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_DOWHILE);
    StmtDoWhile *dowhile = &stmt->dowhile;

    char *start = tcg_loop_tag_label("start", dowhile->label);
    char *cont = tcg_continue_label(dowhile->label);
    char *brk = tcg_break_label(dowhile->label);

    tcg_append(state, tac_label(start, stmt->loc));
    tcg_statement(state, dowhile->body);
    tcg_append(state, tac_label(cont, stmt->loc));
    TacNode *cond = tcg_expression_and_convert(state, dowhile->cond);
    tcg_append(state, tac_jump_not_zero(cond, start, stmt->loc));
    tcg_append(state, tac_label(brk, stmt->loc));

    safe_free(start);
    safe_free(cont);
    safe_free(brk);
}

//
// Generate TAC for a for loop
//
static void tcg_for(TacState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_FOR);
    StmtFor *for_ = &stmt->for_;

    char *start = tcg_loop_tag_label("start", for_->label);
    char *cont = tcg_continue_label(for_->label);
    char *brk = tcg_break_label(for_->label);

    TacNode *init_result = NULL;
    switch (for_->init->tag) {
        case FI_EXPRESSION:     init_result = tcg_expression_and_convert(state, for_->init->exp); break;
        case FI_DECLARATION:    tcg_declaration(state, for_->init->decl); break;
        case FI_NONE:           break;
    }
    //
    // Init result is unused, it is purely for side effects.
    //
    tac_free(init_result);

    tcg_append(state, tac_label(start, stmt->loc));

    //
    // Handle the condition if there is one. If there is none, then the 
    // loop must be exited by some other means.
    //
    if (for_->cond) {
        TacNode *cond = tcg_expression_and_convert(state, for_->cond);
        tcg_append(state, tac_jump_zero(cond, brk, stmt->loc));
    }

    tcg_statement(state, for_->body);
  
    tcg_append(state, tac_label(cont, stmt->loc));

    if (for_->post) {
       TacNode *post_result = tcg_expression_and_convert(state, for_->post);
        //
        // As with init the result is unused.
        //
        tac_free(post_result);
    }

    tcg_append(state, tac_jump(start, stmt->loc));
    tcg_append(state, tac_label(brk, stmt->loc));

    safe_free(start);
    safe_free(cont);
    safe_free(brk);
}

//
// Generate TAC for a break statement.
//
static void tcg_break(TacState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_BREAK);

    char *brk = tcg_break_label(stmt->break_.label);
    tcg_append(state, tac_jump(brk, stmt->loc));
    safe_free(brk);
}

//
// Generate TAC for a continue statement.
//
static void tcg_continue(TacState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_CONTINUE);

    char *cont = tcg_continue_label(stmt->continue_.label);
    tcg_append(state, tac_jump(cont, stmt->loc));
    safe_free(cont);
}

//
// Generate TAC for a switch statement.
//
static void tcg_switch(TacState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_SWITCH);
    StmtSwitch *switch_ = &stmt->switch_;

    TacNode *cond = tcg_expression_and_convert(state, switch_->cond);
    ConstIntSize size = (switch_->cond->type->tag == TT_LONG || switch_->cond->type->tag == TT_ULONG) ? CIS_LONG : CIS_INT;
    ConstIntSign sign = (switch_->cond->type->tag == TT_UINT || switch_->cond->type->tag == TT_ULONG) ? CIS_UNSIGNED : CIS_SIGNED;

    for (ListNode *curr = switch_->cases.head; curr; curr = curr->next) {
        CaseLabel *label = CONTAINER_OF(curr, CaseLabel, list);

        TacNode *cmp = tcg_temporary(state, type_clone(switch_->cond->type), stmt->loc);
        char *case_label = tcg_case_label(switch_->label, label->value);

        Const caseval = const_make_int(size, sign, label->value);

        tcg_append(state, tac_binary(
            BOP_SUBTRACT, 
            tac_clone_operand(cond),
            tac_const(caseval, stmt->loc), 
            cmp,
            stmt->loc));
        tcg_append(state, tac_jump_zero(tac_clone_operand(cmp), case_label, stmt->loc));
        safe_free(case_label);
    }

    char *break_label = tcg_break_label(switch_->label);

    if (switch_->has_default) {
        char *label = tcg_default_label(switch_->label);
        tcg_append(state, tac_jump(label, stmt->loc));
        safe_free(label);
    } else {
        tcg_append(state, tac_jump(break_label, stmt->loc));
    }
    
    tcg_statement(state, switch_->body);

    tcg_append(state, tac_label(break_label, stmt->loc));
}

//
// Generate TAC for a case statement.
//
static void tcg_case(TacState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_CASE);
    StmtCase *case_ = &stmt->case_;

    char *case_label = tcg_case_label(case_->label, case_->value);

    tcg_append(state, tac_label(case_label, stmt->loc));
    tcg_statement(state, case_->stmt);

    safe_free(case_label);
}

//
// Generate TAC for a default statement.
//
static void tcg_default(TacState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_DEFAULT);
    StmtDefault *def = &stmt->default_;

    char *def_label = tcg_default_label(def->label);

    tcg_append(state, tac_label(def_label, stmt->loc));
    tcg_statement(state, def->stmt);

    safe_free(def_label);
}

//
// Generate TAC for a statement.
//
static void tcg_statement(TacState *state, Statement *stmt)
{
    switch (stmt->tag) {
        case STMT_EXPRESSION:   tcg_expression(state, stmt->exp.exp); break;
        case STMT_RETURN:       tcg_return(state, stmt); break;
        case STMT_IF:           tcg_if(state, stmt); break;
        case STMT_NULL:         break;
        case STMT_LABEL:        tcg_label(state, stmt); break;
        case STMT_GOTO:         tcg_goto(state, stmt); break;
        case STMT_COMPOUND:     tcg_compound(state, stmt); break;
        case STMT_WHILE:        tcg_while(state, stmt); break;
        case STMT_FOR:          tcg_for(state, stmt); break;
        case STMT_DOWHILE:      tcg_do_while(state, stmt); break;
        case STMT_BREAK:        tcg_break(state, stmt); break;
        case STMT_CONTINUE:     tcg_continue(state, stmt); break;
        case STMT_SWITCH:       tcg_switch(state, stmt); break;
        case STMT_CASE:         tcg_case(state, stmt); break;
        case STMT_DEFAULT:      tcg_default(state, stmt); break;
    }
}

//
// Generate TAC for a function definition.
//
static TacNode *tcg_funcdef(Declaration *func, SymbolTable *stab, TypeTable *typetab)
{
    ICE_ASSERT(func);
    ICE_ASSERT(func->tag == DECL_FUNCTION);
    
    FileLine loc = func->loc;

    List parms;
    list_clear(&parms);

    for (ListNode *curr = func->func.parms.head; curr; curr = curr->next) {
        FuncParameter *parm = CONTAINER_OF(curr, FuncParameter, list);

        TacNode *var = tac_var(parm->name, loc);
        list_push_back(&parms, &var->list);
    }

    TacState funcstate;
    list_clear(&funcstate.code);
    funcstate.stab = stab;
    funcstate.typetab = typetab;

    tcg_block(&funcstate, func->func.body);

    //
    // Put a `return 0;` at the end of all functions. This gives the 
    // correct behavior for main().
    //
    Const zero = const_make_zero();

    tcg_append(
        &funcstate, 
        tac_return(
            tac_const(zero, loc),
            loc
        )
    );

    Symbol *sym = stab_lookup(stab, func->func.name);
    ICE_ASSERT(sym->tag == ST_FUNCTION);

    return tac_function_def(func->func.name, sym->func.global, parms, funcstate.code, loc);
}

//
// Enumerate the symbol table for static variables.
//
static void tcg_statics(List *code, SymbolTable *stab, TypeTable *typetab)
{
    HashIterator iter;

    for (HashNode *node = hashtab_first(stab->hashtab, &iter); node; node = hashtab_next(&iter)) {
        Symbol *sym = CONTAINER_OF(node, Symbol, hash);
        if (sym->tag == ST_STATIC_VAR) {
            TacNode *var = NULL;
            
            bool global = sym->stvar.global;
            FileLine loc = sym->stvar.loc;
            List zeroes;
            list_clear(&zeroes);

            switch (sym->stvar.siv) {
                case SIV_INIT:      var = tac_static_var(node->key, global, type_clone(sym->type), sym->stvar.initial, loc); break;
                case SIV_NO_INIT:   break;
                case SIV_TENTATIVE:
                    list_push_back(&zeroes, &sinit_make_zero(type_size(typetab, sym->type))->list);
                    var = tac_static_var(node->key, global, type_clone(sym->type), zeroes, loc); 
                    break;
            }

            if (var) {
                list_push_back(code, &var->list);
            }
        } else if (sym->tag == ST_CONSTANT) {
            TacNode *con = NULL;
            FileLine loc = sym->stvar.loc;
            con = tac_static_const(node->key, type_clone(sym->type), sym->stconst, loc);
            list_push_back(code, &con->list);            
        }
    }
}

//
// Top level entry to generate a TAC program from an AST.
//
TacNode *tcg_gen(AstProgram *prog, SymbolTable *stab, TypeTable *typetab)
{
    List funcs;
    list_clear(&funcs);

    //
    // Generate TAC for functions. Global scope variables are separate.
    //
    for (ListNode *curr = prog->decls.head; curr; curr = curr->next) {
        Declaration *decl = CONTAINER_OF(curr, Declaration, list);

        if (decl->tag == DECL_FUNCTION && decl->func.has_body) {
            TacNode *func = tcg_funcdef(decl, stab, typetab);
            list_push_back(&funcs, &func->list);
        }
    }

    tcg_statics(&funcs, stab, typetab);

    return tac_program(funcs, prog->loc);
}
