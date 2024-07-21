#include "tacgen.h"

#include "errors.h"
#include "ice.h"
#include "list.h"
#include "safemem.h"
#include "tacnode.h"

typedef struct 
{
    List code;              // list of TacNode *
} TacState;

static TacNode *tcg_expression(TacState *state, Expression *exp);

//
// NOTE TAC generation expects a syntactically correct program from
//      the parser; hence, we use ICE_ASSERT to check most errors.
//

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
static TacNode *tcg_temporary(FileLine loc)
{
    static int suffix = 0;

    char *name = saprintf("tmp.%d", suffix++);
    TacNode *var = tac_var(name, loc);
    safe_free(name);

    return var;
}

//
// Return a globally unique label.
//
static TacNode *tcg_make_label(FileLine loc)
{
    static int suffix = 0;

    char *name = saprintf("label.%d", suffix++);
    TacNode *var = tac_label(name, loc);
    safe_free(name);

    return var;
}

//
// Return TAC for a unary operation.
//
static TacNode *tcg_unary_op(TacState *state, Expression *unary)
{
    TacNode *src = tcg_expression(state, unary->unary.exp);
    TacNode *dst = tcg_temporary(unary->loc);

    tcg_append(state, tac_unary(unary->unary.op, src, dst, unary->loc));

    return tac_var(dst->var.name, unary->loc);
}

//
// Return TAC for a short-circuit logical operator; i.e. && or ||
//
static TacNode *tcg_short_circuit_op(TacState *state, Expression *binary)
{
    ICE_ASSERT(binary->tag == EXP_BINARY);
    ICE_ASSERT(binary->binary.op == BOP_LOGAND || binary->binary.op == BOP_LOGOR);

    FileLine loc = binary->loc;
    bool is_and = binary->binary.op == BOP_LOGAND;
    int success_val = is_and ? 1 : 0;
    int fail_val = is_and ? 0 : 1;

    TacNode *(*tac_sc_jump)(TacNode *, char *, FileLine) = is_and ? tac_jump_zero : tac_jump_not_zero;

    TacNode *tf_label = tcg_make_label(loc);
    TacNode *end_label = tcg_make_label(loc);
    TacNode *result = tcg_temporary(loc);
    
    TacNode *left = tcg_expression(state, binary->binary.left);
    tcg_append(state, tac_sc_jump(left, tf_label->label.name, loc));
    TacNode *right = tcg_expression(state, binary->binary.right);
    tcg_append(state, tac_sc_jump(right, tf_label->label.name, loc));
    tcg_append(state, tac_copy(tac_const_int(success_val, loc), tac_var(result->var.name, loc), loc));
    tcg_append(state, tac_jump(end_label->label.name, loc));
    tcg_append(state, tf_label);
    tcg_append(state, tac_copy(tac_const_int(fail_val, loc), tac_var(result->var.name, loc), loc));
    tcg_append(state, end_label);

    return result;
}

//
// Return TAC for a binary expression.
// 
static TacNode *tcg_binary_op(TacState *state, Expression *binary)
{
    ICE_ASSERT(binary->tag == EXP_BINARY);

    //
    // Special handling for operators which require short-circuit evaluation
    //
    if (binary->binary.op == BOP_LOGAND || binary->binary.op == BOP_LOGOR) {
        return tcg_short_circuit_op(state, binary);
    }

    TacNode *left = tcg_expression(state, binary->binary.left);
    TacNode *right = tcg_expression(state, binary->binary.right);
    TacNode *dst = tcg_temporary(binary->loc);

    tcg_append(state, tac_binary(binary->binary.op, left, right, dst, binary->loc));

    return tac_var(dst->var.name, binary->loc);
}

//
// Generate TAC for an expression
//
static TacNode *tcg_expression(TacState *state, Expression *exp)
{
    switch (exp->tag) {
        case EXP_INT:       return tac_const_int(exp->intval, exp->loc);
        case EXP_UNARY:     return tcg_unary_op(state, exp);
        case EXP_BINARY:    return tcg_binary_op(state, exp);
    }

    ICE_ASSERT(((void)"invalid expression node", false));
    
    //
    // never reached
    //
    return NULL;
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
        exp = tcg_expression(state, ret->ret.exp);
    }

    tcg_append(state, tac_return(exp, loc));
}

//
// Generate TAC for a function definition.
//
static TacNode *tcg_funcdef(TacState *state, AstNode *func)
{
    ICE_ASSERT(func);
    ICE_ASSERT(func->tag == AST_FUNCTION);
    ICE_ASSERT(func->func.stmt);

    FileLine loc = func->loc;

    TacState funcstate;
    list_clear(&funcstate.code);

    tcg_return(&funcstate, func->func.stmt);

    return tac_function_def(func->func.name, funcstate.code, loc);
}

//
// Top level entry to generate a TAC program from an AST.
//
TacNode *tcg_gen(AstNode *ast)
{
    ICE_ASSERT(ast != NULL);
    ICE_ASSERT(ast->tag == AST_PROGRAM);

    TacState state;

    list_clear(&state.code);

    FileLine loc = ast->loc;

    TacNode *funcdef = tcg_funcdef(&state, ast->prog.func);

    return tac_program(funcdef, loc);
}