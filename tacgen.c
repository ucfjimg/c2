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
// Return TAC for a unary operation.
//
static TacNode *tcg_unary_op(TacState *state, Expression *unary)
{
    FileLine loc = unary->loc;

    TacNode *src = tcg_expression(state, unary->unary.exp);
    TacNode *dst = tcg_temporary(unary->loc);

    tcg_append(state, tac_unary(unary->unary.op, src, dst, loc));

    return tac_var(dst->var.name, loc);
}

//
// Generate TAC for an expression
//
static TacNode *tcg_expression(TacState *state, Expression *exp)
{
    switch (exp->tag) {
        case EXP_INT:       return tac_const_int(exp->intval, exp->loc);
        case EXP_UNARY:     return tcg_unary_op(state, exp);
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