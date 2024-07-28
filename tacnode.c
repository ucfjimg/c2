#include "tacnode.h"

#include "ice.h"
#include "safemem.h"

#include <stdio.h>

static void tac_print_recurse(TacNode *tac, int tab, bool locs);

//
// Allocator for all TAC nodes.
//
static TacNode *tac_alloc(TacTag tag, FileLine loc)
{
    TacNode *tac = safe_zalloc(sizeof(TacNode));
    tac->tag = tag;
    tac->loc = loc;
    return tac;
}

//
// Constructor for a TAC program.
//
TacNode *tac_program(List decls, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_PROGRAM, loc);
    tac->prog.decls = decls;
    return tac;
}

//
// Free a TAC program.
//
static void tac_program_free(TacProgram *prog)
{
    for (ListNode *curr = prog->decls.head; curr; ) {
        ListNode *next = curr->next;
        TacNode *decl = CONTAINER_OF(curr, TacNode, list);
        tac_free(decl);
        curr = next;
    }
}

//
// Constructor for a TAC function definition.
//
TacNode *tac_function_def(char *name, List parms, List body, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_FUNCDEF, loc);
    tac->funcdef.name = safe_strdup(name);
    tac->funcdef.parms = parms;
    tac->funcdef.body = body;
    return tac;
}

//
// Free a TAC function parameter.
//
static void tac_func_parm_free(TacFuncParam *parm)
{
    safe_free(parm->name);
    safe_free(parm);
}

//
// Free a TAC function definition.
//
static void tac_function_def_free(TacFuncDef *funcdef)
{
    safe_free(funcdef->name);

    for (ListNode *curr = funcdef->parms.head; curr; ) {
        ListNode *next = curr->next;

        TacFuncParam *parm = CONTAINER_OF(curr, TacFuncParam, list);
        tac_func_parm_free(parm);

        curr = next;
    }

    for (ListNode *curr = funcdef->body.head; curr; ) {
        ListNode *next = curr->next;

        TacNode *node = CONTAINER_OF(curr, TacNode, list);
        tac_free(node);

        curr = next;
    }

}

//
// Constructor for a TAC return statement.
//
// The value, if it exists, must be a constant or a variable
//
TacNode *tac_return(TacNode *val, FileLine loc)
{
    ICE_ASSERT(val == NULL || (val->tag == TAC_CONST_INT || val->tag == TAC_VAR));

    TacNode *tac = tac_alloc(TAC_RETURN, loc);
    tac->ret.val = val;
    return tac;
}

//
// Free a TAC return statement.
//
static void tac_return_free(TacReturn *ret)
{
    tac_free(ret->val);
}

//
// Constructor for a TAC copy instruction.
//
// The source must be a variable or a constant; the destination must
// be a variable.
//
TacNode *tac_copy(TacNode *src, TacNode *dst, FileLine loc)
{
    ICE_ASSERT(src->tag == TAC_CONST_INT || src->tag == TAC_VAR);
    ICE_ASSERT(dst->tag == TAC_VAR);

    TacNode *tac = tac_alloc(TAC_COPY, loc);

    tac->copy.src = src;
    tac->copy.dst = dst;
    return tac;
}

//
// Free a TAC copy instruction.
//
static void tac_copy_free(TacCopy *copy)
{
    tac_free(copy->src);
    tac_free(copy->dst);
}

//
// Constructor for a TAC jump instruction.
//
TacNode *tac_jump(char *target, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_JUMP, loc);
    tac->jump.target = safe_strdup(target);
    return tac;
}

//
// Free a TAC jump instruction.
//
static void tac_jump_free(TacJump *jump)
{
    safe_free(jump->target);
}

//
// Constructor for a TAC jump-if-zero instruction.
//
TacNode *tac_jump_zero(TacNode *condition, char *target, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_JUMP_ZERO, loc);
    tac->jump_zero.condition = condition;
    tac->jump_zero.target = safe_strdup(target);
    return tac;
}

//
// Free a TAC jump-if-zero instruction.
//
static void tac_jump_zero_free(TacJumpZero *jump)
{
    tac_free(jump->condition);
    safe_free(jump->target);
}

//
// Constructor for a TAC jump-if-not-zero instruction.
//
TacNode *tac_jump_not_zero(TacNode *condition, char *target, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_JUMP_NOT_ZERO, loc);
    tac->jump_not_zero.condition = condition;
    tac->jump_not_zero.target = safe_strdup(target);
    return tac;
}

//
// Free a TAC jump-if-not-zero instruction.
//
static void tac_jump_not_zero_free(TacJumpNotZero *jump)
{
    tac_free(jump->condition);
    safe_free(jump->target);
}

//
// Constructor for a TAC label.
//
TacNode *tac_label(char *name, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_LABEL, loc);
    tac->label.name = safe_strdup(name);
    return tac;
}

//
// Free a TAC label node.
//
static void tac_label_free(TacLabel *label)
{
    safe_free(label->name);
}

//
// Constructor for a TAC unary operator.
//
// The source must be a variable or a constant; the destination must
// be a variable.
//
TacNode *tac_unary(UnaryOp op, TacNode *src, TacNode *dst, FileLine loc)
{
    ICE_ASSERT(src->tag == TAC_CONST_INT || src->tag == TAC_VAR);
    ICE_ASSERT(dst->tag == TAC_VAR);

    TacNode *tac = tac_alloc(TAC_UNARY, loc);
    tac->unary.op = op;
    tac->unary.src = src;
    tac->unary.dst = dst;
    return tac;
}

//
// Free a TAC unary operator.
//
static void tac_unary_free(TacUnary *unary)
{
    tac_free(unary->src);
    tac_free(unary->dst);
}

//
// Constructor for a TAC binary operator.
//
// The sources must be a variable or a constant; the destination must
// be a variable.
//
TacNode *tac_binary(BinaryOp op, TacNode *left, TacNode *right, TacNode *dst, FileLine loc)
{
    ICE_ASSERT(left->tag == TAC_CONST_INT || left->tag == TAC_VAR);
    ICE_ASSERT(right->tag == TAC_CONST_INT || right->tag == TAC_VAR);
    ICE_ASSERT(dst->tag == TAC_VAR);

    TacNode *tac = tac_alloc(TAC_BINARY, loc);
    tac->binary.op = op;
    tac->binary.left = left;
    tac->binary.right = right;
    tac->binary.dst = dst;
    return tac;
}

//
// Free a TAC binary operator.
//
static void tac_binary_free(TacBinary *binary)
{
    tac_free(binary->left);
    tac_free(binary->right);
    tac_free(binary->dst);
}

//
// Construct a TAC constant integer.
//
TacNode *tac_const_int(unsigned long val, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_CONST_INT, loc);
    tac->constint.val = val;
    return tac;
}

//
// Constructor for a TAC variable.
//
TacNode *tac_var(char *name, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_VAR, loc);
    tac->var.name = safe_strdup(name);
    return tac;
}

//
// Free a TAC variable.
//
static void tac_var_free(TacVar *var)
{
    safe_free(var->name);
}

//
// Constructor for a TAC function call.
//
TacNode *tac_function_call(char *name, List args, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_FUNCTION_CALL, loc);

    tac->call.name = safe_strdup(name);
    tac->call.args = args;
    tac->call.dst = dst;

    return tac;
}

//
// Free a TAC function call.
//
static void tac_function_call_free(TacFunctionCall *call)
{
    safe_free(call->name);

    for (ListNode *curr = call->args.head; curr; ) {
        ListNode *next = curr->next;

        TacNode *arg = CONTAINER_OF(curr, TacNode, list);
        tac_free(arg);

        curr = next;
    }

    tac_free(call->dst);
}

//
// Clone and operand node. The operand must be of type
// TAC_VAR or TAC_CONST_INT.
//
TacNode *tac_clone_operand(TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_VAR || tac->tag == TAC_CONST_INT);

    switch (tac->tag) {
        case TAC_VAR:       return tac_var(tac->var.name, tac->loc);
        case TAC_CONST_INT: return tac_const_int(tac->constint.val, tac->loc);
    
        default:
            return tac_const_int(0, tac->loc);
    }
}


//
// Free a TAC tree.
//
void tac_free(TacNode *tac)
{
    if (tac) {
        switch (tac->tag) {
            case TAC_PROGRAM:       tac_program_free(&tac->prog); break;
            case TAC_FUNCDEF:       tac_function_def_free(&tac->funcdef); break;
            case TAC_CONST_INT:     break;
            case TAC_RETURN:        tac_return_free(&tac->ret); break;
            case TAC_COPY:          tac_copy_free(&tac->copy); break;
            case TAC_JUMP:          tac_jump_free(&tac->jump); break;
            case TAC_JUMP_ZERO:     tac_jump_zero_free(&tac->jump_zero); break;
            case TAC_JUMP_NOT_ZERO: tac_jump_not_zero_free(&tac->jump_not_zero); break;
            case TAC_LABEL:         tac_label_free(&tac->label); break;
            case TAC_UNARY:         tac_unary_free(&tac->unary); break;
            case TAC_BINARY:        tac_binary_free(&tac->binary); break;
            case TAC_VAR:           tac_var_free(&tac->var); break;
            case TAC_FUNCTION_CALL: tac_function_call_free(&tac->call); break;
        }

        safe_free(tac);
    }
}

//
// Print a TAC location as an indented comment.
//
static void tac_print_location(FileLine loc, int tab)
{
    char *locdesc = fileline_describe(&loc);
    printf("%*s/* %s */\n", tab, "", locdesc);
    safe_free(locdesc);
}

//
// Print a TAC program.
//
static void tac_print_program(TacProgram *prog, int tab, bool locs)
{
    printf("%*sprogram() {\n", tab, "");
    for (ListNode *curr = prog->decls.head; curr; curr = curr->next) {
        TacNode *decl = CONTAINER_OF(curr, TacNode, list);
        tac_print_recurse(decl, tab + 2, locs);
    }
    printf("%*s}\n", tab, "");
}

//
// Print a TAC function definition.
//
static void tac_print_funcdef(TacFuncDef *funcdef, int tab, bool locs)
{
    printf("%*sfunction(%s) (", tab, "", funcdef->name);

    for (ListNode *curr = funcdef->parms.head; curr; curr = curr->next) {
        TacFuncParam *parm = CONTAINER_OF(curr, TacFuncParam, list);
        printf("%s", parm->name);
        if (curr->next) {
            printf(", ");
        }
    }

    printf(") {\n");

    for (ListNode *node = funcdef->body.head; node; node = node->next) {
        TacNode *tac = CONTAINER_OF(node, TacNode, list);
        tac_print_recurse(tac, tab + 2, locs);
    }

    printf("%*s}\n", tab, "");
}

//
// Print a TAC return statement.
//
static void tac_print_return(TacReturn *ret, int tab, bool locs)
{
    printf("%*sreturn() {\n", tab, "");
    tac_print_recurse(ret->val, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a TAC copy instruction.
//
static void tac_print_copy(TacCopy *copy, int tab, bool locs)
{
    printf("%*scopy() {\n", tab, "");
    tac_print_recurse(copy->src, tab + 2, locs);
    printf("%*s}, {\n", tab, "");
    tac_print_recurse(copy->dst, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a TAC jump instruction.
//
static void tac_print_jump(TacJump *jump, int tab, bool locs)
{
    printf("%*sjump(%s);\n", tab, "", jump->target);
}

//
// Print a TAC jump-if-zero instruction.
//
static void tac_print_jump_zero(TacJumpZero *jump, int tab, bool locs)
{
    printf("%*sjump-zero(%s) if {\n", tab, "", jump->target);
    tac_print_recurse(jump->condition, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a TAC jump-if-not=zero instruction.
//
static void tac_print_jump_not_zero(TacJumpNotZero *jump, int tab, bool locs)
{
    printf("%*sjump-not-zero(%s) if {\n", tab, "", jump->target);
    tac_print_recurse(jump->condition, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a TAC label.
//
static void tac_print_label(TacLabel *label, int tab)
{
    printf("%*slabel(%s);\n", tab, "", label->name);
}

//
// Print a TAC unary operator.
//
static void tac_print_unary(TacUnary *unary, int tab, bool locs)
{
    printf("%*sunary(%s) {\n", tab, "", uop_describe(unary->op));
    tac_print_recurse(unary->src, tab + 2, locs);
    printf("%*s,\n", tab + 2, "");
    tac_print_recurse(unary->dst, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a TAC binary operator.
//
static void tac_print_binary(TacBinary *binary, int tab, bool locs)
{
    printf("%*sbinary {(\n", tab, "");
    tac_print_recurse(binary->left, tab + 2, locs);
    printf("%*s) %s (\n", tab + 2, "", bop_describe(binary->op));
    tac_print_recurse(binary->right, tab + 2, locs);
    printf("%*s) => (\n", tab + 2, "");
    tac_print_recurse(binary->dst, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a TAC integer constant.
//
static void tac_print_const_int(TacConstInt *constint, int tab)
{
    printf("%*sconst-int(%lu)\n", tab, "", constint->val);
}

//
// Print a TAC variable.
//
static void tac_print_var(TacVar *var, int tab)
{
    printf("%*svar(%s)\n", tab, "", var->name);
}

//
// Print a TAC function call.
//
static void tac_print_function_call(TacFunctionCall *call, int tab, bool locs)
{
    printf("%*scall(%s) {\n", tab, "", call->name);

    for (ListNode *curr = call->args.head; curr; curr = curr->next) {
        TacNode *arg = CONTAINER_OF(curr, TacNode, list);
        tac_print_recurse(arg, tab + 2, locs);
    
        if (curr->next) {
            printf("%*s,\n", tab + 2, "");
        }
    }

    printf("%*s} -> {\n", tab, "");
    tac_print_recurse(call->dst, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a node of a TAC tree.
//
static void tac_print_recurse(TacNode *tac, int tab, bool locs)
{
    if (locs) {
        tac_print_location(tac->loc, tab);
    }
    switch (tac->tag) {
        case TAC_PROGRAM:       tac_print_program(&tac->prog, tab, locs); break;
        case TAC_FUNCDEF:       tac_print_funcdef(&tac->funcdef, tab, locs); break;
        case TAC_RETURN:        tac_print_return(&tac->ret, tab, locs); break;
        case TAC_COPY:          tac_print_copy(&tac->copy, tab, locs); break;
        case TAC_JUMP:          tac_print_jump(&tac->jump, tab, locs); break;
        case TAC_JUMP_ZERO:     tac_print_jump_zero(&tac->jump_zero, tab, locs); break;
        case TAC_JUMP_NOT_ZERO: tac_print_jump_not_zero(&tac->jump_not_zero, tab, locs); break;
        case TAC_LABEL:         tac_print_label(&tac->label, tab); break;
        case TAC_UNARY:         tac_print_unary(&tac->unary, tab, locs); break;
        case TAC_BINARY:        tac_print_binary(&tac->binary, tab, locs); break;
        case TAC_CONST_INT:     tac_print_const_int(&tac->constint, tab); break;
        case TAC_VAR:           tac_print_var(&tac->var, tab); break;
        case TAC_FUNCTION_CALL: tac_print_function_call(&tac->call, tab, locs); break;

        default:
            printf("%*s<invalid-TAC-tag>();\n", tab, "");
            break;
    }
}

//
// Print a TAC tree, with location information if `locs` is true.
//
void tac_print(TacNode *tac, bool locs)
{
    tac_print_recurse(tac, 0, locs);
}
