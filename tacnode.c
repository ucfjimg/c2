#include "tacnode.h"

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
TacNode *tac_program(TacNode *func, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_PROGRAM, loc);
    tac->prog.func = func;
    return tac;
}

//
// Free a TAC program.
//
static void tac_program_free(TacProgram *prog)
{
    tac_free(prog->func);
}

//
// Constructor for a TAC function definition.
//
TacNode *tac_function_def(char *name, List body, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_FUNCDEF, loc);
    tac->funcdef.name = safe_strdup(name);
    tac->funcdef.body = body;
    return tac;
}

//
// Free a TAC function definition.
//
static void tac_function_def_free(TacFuncDef *funcdef)
{
    safe_free(funcdef->name);

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
TacNode *tac_return(TacNode *val, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_RETURN, loc);
    tac->ret.val = val;
    return tac;
}

//
// Free a TAC return statement.
//
void tac_return_free(TacReturn *ret)
{
    tac_free(ret->val);
}

//
// Constructor for a TAC unary operator.
//
TacNode *tac_unary(UnaryOp op, TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_UNARY, loc);
    tac->unary.op = op;
    tac->unary.src = src;
    tac->unary.dst = dst;
    return tac;
}

//
// Free a TAC unary operator.
//
void tac_unary_free(TacUnary *unary)
{
    tac_free(unary->src);
    tac_free(unary->dst);
}

//
// Constructor for a TAC binary operator.
//
TacNode *tac_binary(BinaryOp op, TacNode *left, TacNode *right, TacNode *dst, FileLine loc)
{
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
void tac_binary_free(TacBinary *binary)
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
// Free a TAC tree.
//
void tac_free(TacNode *tac)
{
    if (tac) {
        switch (tac->tag) {
            case TAC_PROGRAM: tac_program_free(&tac->prog); break;
            case TAC_FUNCDEF: tac_function_def_free(&tac->funcdef); break;
            case TAC_RETURN:  tac_return_free(&tac->ret); break;
            case TAC_UNARY:   tac_unary_free(&tac->unary); break;
            case TAC_BINARY:  tac_binary_free(&tac->binary); break;
            case TAC_VAR:     tac_var_free(&tac->var); break;

        default:
            break;
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
    tac_print_recurse(prog->func, tab + 2, locs);
    printf("%*s{\n", tab, "");
}

//
// Print a TAC function definition.
//
static void tac_print_funcdef(TacFuncDef *funcdef, int tab, bool locs)
{
    printf("%*sfunction(%s) {\n", tab, "", funcdef->name);

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
// Print a node of a TAC tree.
//
static void tac_print_recurse(TacNode *tac, int tab, bool locs)
{
    if (locs) {
        tac_print_location(tac->loc, tab);
    }
    switch (tac->tag) {
        case TAC_PROGRAM:   tac_print_program(&tac->prog, tab, locs); break;
        case TAC_FUNCDEF:   tac_print_funcdef(&tac->funcdef, tab, locs); break;
        case TAC_RETURN:    tac_print_return(&tac->ret, tab, locs); break;
        case TAC_UNARY:     tac_print_unary(&tac->unary, tab, locs); break;
        case TAC_BINARY:    tac_print_binary(&tac->binary, tab, locs); break;
        case TAC_CONST_INT: tac_print_const_int(&tac->constint, tab); break;
        case TAC_VAR:       tac_print_var(&tac->var, tab); break;

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
