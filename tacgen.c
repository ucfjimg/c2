#include "tacgen.h"

#include "errors.h"
#include "hashtab.h"
#include "ice.h"
#include "list.h"
#include "safemem.h"
#include "symtab.h"
#include "tacnode.h"

typedef struct 
{
    List code;              // list of TacNode *
    SymbolTable *stab;      // symbol table
} TacState;

static TacNode *tcg_expression(TacState *state, Expression *exp);
static void tcg_statement(TacState *state, Statement *stmt);

//
// NOTE TAC generation expects a syntactically correct program from
//      the parser; hence, we use ICE_ASSERT to check most errors.
//

//
// Generated names. Note that for user-defined names, a suffix of
// .# is added where # is a globally unique id. For generated names,
// the suffix always has two .'s (i.e. ..#) so the two spaces can
// never collide. 
//
// . is a valid label character for the assembler but not for a 
// C identifier.
//

//
// Format a loop labvel with a unique tag. Returns an 
// allocated string.
//
static char *tcg_loop_tag_label(char *tag, int label)
{
    ICE_ASSERT(label >= 0);

    return saprintf("loop_%s..%d", tag, label);
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
static char *tcg_case_label(int label, int value)
{
    char *tag = saprintf("case_%x", value);
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
    static int suffix = 0;

    char *name = saprintf("tmp..%d", suffix++);
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
    static int suffix = 0;

    char *name = saprintf("label..%d", suffix++);
    TacNode *var = tac_label(name, loc);
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
    TacNode *dst = tcg_temporary(state, type_clone(unary->type), loc);

    if (unary->unary.op == UOP_PREDECREMENT || unary->unary.op == UOP_PREINCREMENT) {
        TacNode *tmp = tcg_temporary(state, type_clone(unary->type), loc);
        TacNode *operand = tcg_expression(state, unary->unary.exp);
                
        if (unary->unary.op == UOP_PREDECREMENT) {
            tcg_append(state, tac_binary(BOP_SUBTRACT, operand, tac_const_int(1, false, loc), tmp, loc));
        } else {
            tcg_append(state, tac_binary(BOP_ADD, operand, tac_const_int(1, false, loc), tmp, loc));
        }

        tcg_append(state, tac_copy(tac_clone_operand(tmp), tac_clone_operand(operand), loc));

        return tac_clone_operand(operand);
    }

    if (unary->unary.op == UOP_POSTDECREMENT || unary->unary.op == UOP_POSTINCREMENT) {
        TacNode *oldval = tcg_temporary(state, type_clone(unary->type), loc);
        TacNode *tmp = tcg_temporary(state, type_clone(unary->type), loc);
        TacNode *operand = tcg_expression(state, unary->unary.exp);
                
        tcg_append(state, tac_copy(tac_clone_operand(operand), oldval, loc));

        if (unary->unary.op == UOP_POSTDECREMENT) {
            tcg_append(state, tac_binary(BOP_SUBTRACT, operand, tac_const_int(1, false, loc), tmp, loc));
        } else {
            tcg_append(state, tac_binary(BOP_ADD, operand, tac_const_int(1, false, loc), tmp, loc));
        }

        tcg_append(state, tac_copy(tac_clone_operand(tmp), tac_clone_operand(operand), loc));

        return tac_clone_operand(oldval);
    }

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
    TacNode *result = tcg_temporary(state, type_clone(binary->type), loc);
    
    TacNode *left = tcg_expression(state, binary->binary.left);
    tcg_append(state, tac_sc_jump(left, tf_label->label.name, loc));
    TacNode *right = tcg_expression(state, binary->binary.right);
    tcg_append(state, tac_sc_jump(right, tf_label->label.name, loc));
    tcg_append(state, tac_copy(tac_const_int(success_val, false, loc), tac_var(result->var.name, loc), loc));
    tcg_append(state, tac_jump(end_label->label.name, loc));
    tcg_append(state, tf_label);
    tcg_append(state, tac_copy(tac_const_int(fail_val, false, loc), tac_var(result->var.name, loc), loc));
    tcg_append(state, end_label);

    return result;
}

//
// Generate TAC for a binary expression.
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
    TacNode *dst = tcg_temporary(state, type_clone(binary->type), binary->loc);

    tcg_append(state, tac_binary(binary->binary.op, left, right, dst, binary->loc));

    return tac_var(dst->var.name, binary->loc);
}

//
// Generate TAC for a conditional expression.
//
static TacNode *tcg_conditional(TacState *state, Expression *condexp)
{
    ICE_ASSERT(condexp->tag == EXP_CONDITIONAL);
    ExpConditional *cond = &condexp->conditional;

    TacNode *falsepart = tcg_make_label(cond->falseval->loc);
    TacNode *end = tcg_make_label(cond->falseval->loc);
    TacNode *result = tcg_temporary(state, type_clone(condexp->type), cond->cond->loc);

    TacNode *condval = tcg_expression(state, cond->cond);
    tcg_append(state, tac_jump_zero(condval, falsepart->label.name, cond->cond->loc));
    TacNode *trueval = tcg_expression(state, cond->trueval);
    tcg_append(state, tac_copy(trueval, tac_clone_operand(result), cond->trueval->loc));
    tcg_append(state, tac_jump(end->label.name, cond->trueval->loc));
    tcg_append(state, tac_label(falsepart->label.name, cond->falseval->loc));
    TacNode *falseval = tcg_expression(state, cond->falseval);
    tcg_append(state, tac_copy(falseval, tac_clone_operand(result), cond->falseval->loc));
    tcg_append(state, tac_label(end->label.name, cond->falseval->loc));

    return result;
}

//
// Generate TAC for an assignment.
//
struct TacNode *tcg_assignment(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_ASSIGNMENT);
    ExpAssignment *assign = &exp->assignment;

    TacNode *left = tcg_expression(state, assign->left);
    TacNode *right = tcg_expression(state, assign->right);

    if (assign->op == BOP_ASSIGN) {
       tcg_append(state, tac_copy(right, left, exp->loc));
    } else {
        ICE_ASSERT(bop_is_compound_assign(assign->op));
        BinaryOp bop = bop_compound_to_binop(assign->op);
        TacNode *tmp = tcg_temporary(state, type_clone(exp->type), exp->loc);
        
        tcg_append(state, tac_binary(bop, left, right, tmp, exp->loc));
        tcg_append(state, tac_copy(tac_clone_operand(tmp), tac_clone_operand(left), exp->loc));
    }

    ICE_ASSERT(left->tag == TAC_VAR);
    return tac_clone_operand(left);
}

//
// Generate TAC for a function call.
//
static TacNode *tcg_function_call(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_FUNCTION_CALL);
    ExpFunctionCall *call = &exp->call;

    List args;
    list_clear(&args);

    for (ListNode *curr = call->args.head; curr; curr = curr->next) {
        Expression *arg = CONTAINER_OF(curr, Expression, list);
        TacNode *tacarg = tcg_expression(state, arg);
        list_push_back(&args, &tacarg->list);
    }

    TacNode *result = tcg_temporary(state, type_clone(exp->type), exp->loc);

    tcg_append(state, tac_function_call(call->name, args, result, exp->loc));

    return tac_clone_operand(result);
}

//
// Generate TAC for a constant integer.
//
static TacNode *tcg_const_int(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_INT);
    return tac_const_int(exp->intval, false, exp->loc);
}

//
// Generate TAC for a constant long.
//
static TacNode *tcg_const_long(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_LONG);
    return tac_const_int(exp->intval, true, exp->loc);
}

//
// Generata TAC for cast.
//
static TacNode *tcg_cast(TacState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_CAST);
    ExpCast *cast = &exp->cast;

    TacNode *inner = tcg_expression(state, cast->exp);
    if (types_equal(cast->exp->type, cast->type)) {
        return inner;
    }

    TacNode *tmp = tcg_temporary(state, type_clone(cast->type), exp->loc);

    if (cast->type->tag == TT_LONG) {
        tcg_append(state, tac_sign_extend(inner, tmp, exp->loc));
    } else {
        tcg_append(state, tac_truncate(inner, tmp, exp->loc));
    }

    return tac_clone_operand(tmp);
}

//
// Generate TAC for an expression.
//
static TacNode *tcg_expression(TacState *state, Expression *exp)
{
    switch (exp->tag) {
        case EXP_INT:           return tcg_const_int(state, exp);
        case EXP_LONG:          return tcg_const_long(state, exp);
        case EXP_VAR:           return tac_var(exp->var.name, exp->loc);
        case EXP_UNARY:         return tcg_unary_op(state, exp);
        case EXP_BINARY:        return tcg_binary_op(state, exp);
        case EXP_CONDITIONAL:   return tcg_conditional(state, exp);
        case EXP_ASSIGNMENT:    return tcg_assignment(state, exp);
        case EXP_FUNCTION_CALL: return tcg_function_call(state, exp); break; 
        case EXP_CAST:          return tcg_cast(state, exp); break;
    }

    ICE_ASSERT(((void)"invalid expression node", false));
    
    //
    // never reached
    //
    return NULL;
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
        TacNode *initval = tcg_expression(state, decl->var.init);
        TacNode *declvar = tac_var(decl->var.name, decl->loc);
        tcg_append(state, tac_copy(initval, declvar, decl->loc));
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
        exp = tcg_expression(state, ret->ret.exp);
    }

    tcg_append(state, tac_return(exp, loc));
}

//
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

    TacNode *cond = tcg_expression(state, ifstmt->ifelse.condition);
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
    TacNode *cond = tcg_expression(state, while_->cond);
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
    TacNode *cond = tcg_expression(state, dowhile->cond);
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
        case FI_EXPRESSION:     init_result = tcg_expression(state, for_->init->exp); break;
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
        TacNode *cond = tcg_expression(state, for_->cond);
        tcg_append(state, tac_jump_zero(cond, brk, stmt->loc));
    }

    tcg_statement(state, for_->body);
  
    tcg_append(state, tac_label(cont, stmt->loc));

    if (for_->post) {
       TacNode *post_result = tcg_expression(state, for_->post);
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

    TacNode *cond = tcg_expression(state, switch_->cond);
    bool is_long = switch_->cond->type->tag == TT_LONG;

    for (ListNode *curr = switch_->cases.head; curr; curr = curr->next) {
        CaseLabel *label = CONTAINER_OF(curr, CaseLabel, list);

        TacNode *cmp = tcg_temporary(state, type_clone(switch_->cond->type), stmt->loc);
        char *case_label = tcg_case_label(switch_->label, label->value);

        tcg_append(state, tac_binary(
            BOP_SUBTRACT, 
            tac_clone_operand(cond),
            tac_const_int(label->value, is_long, stmt->loc), 
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
static TacNode *tcg_funcdef(Declaration *func, SymbolTable *stab)
{
    ICE_ASSERT(func);
    ICE_ASSERT(func->tag == DECL_FUNCTION);
    
    FileLine loc = func->loc;

    List parms;
    list_clear(&parms);

    for (ListNode *curr = func->func.parms.head; curr; curr = curr->next) {
        FuncParameter *parm = CONTAINER_OF(curr, FuncParameter, list);

        TacFuncParam * tparm = safe_zalloc(sizeof(TacFuncParam));
        tparm->name = safe_strdup(parm->name);

        list_push_back(&parms, &tparm->list);
    }

    TacState funcstate;
    list_clear(&funcstate.code);
    funcstate.stab = stab;

    tcg_block(&funcstate, func->func.body);

    //
    // Put a `return 0;` at the end of all functions. This gives the 
    // correct behavior for main().
    //
    tcg_append(
        &funcstate, 
        tac_return(
            tac_const_int(0, false, loc),
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
static void tcg_statics(List *code, SymbolTable *stab)
{
    HashIterator iter;

    for (HashNode *node = hashtab_first(stab->hashtab, &iter); node; node = hashtab_next(&iter)) {
        Symbol *sym = CONTAINER_OF(node, Symbol, hash);
        if (sym->tag == ST_STATIC_VAR) {
            TacNode *var = NULL;
            
            bool global = sym->stvar.global;
            FileLine loc = sym->stvar.loc;

            StaticVarInit init;
            init.value = 0;
            init.is_long = 0;

            switch (sym->stvar.siv) {
                case SIV_INIT:      var = tac_static_var(node->key, global, type_clone(sym->type), sym->stvar.initial, loc); break;
                case SIV_NO_INIT:   break;
                case SIV_TENTATIVE: var = tac_static_var(node->key, global, type_clone(sym->type), init, loc); break;
            }

            if (var) {
                list_push_back(code, &var->list);
            }
        }
    }
}

//
// Top level entry to generate a TAC program from an AST.
//
TacNode *tcg_gen(AstProgram *prog, SymbolTable *stab)
{
    List funcs;
    list_clear(&funcs);

    //
    // Generate TAC for functions. Global scope variables are separate.
    //
    for (ListNode *curr = prog->decls.head; curr; curr = curr->next) {
        Declaration *decl = CONTAINER_OF(curr, Declaration, list);

        if (decl->tag == DECL_FUNCTION && decl->func.has_body) {
            TacNode *func = tcg_funcdef(decl, stab);
            list_push_back(&funcs, &func->list);
        }
    }

    tcg_statics(&funcs, stab);

    return tac_program(funcs, prog->loc);
}
