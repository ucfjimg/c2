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
TacNode *tac_function_def(char *name, bool global, List parms, List body, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_FUNCDEF, loc);
    tac->funcdef.name = safe_strdup(name);
    tac->funcdef.global = global;
    tac->funcdef.parms = parms;
    tac->funcdef.body = body;
    return tac;
}

//
// Free a TAC function definition.
//
static void tac_function_def_free(TacFuncDef *funcdef)
{
    safe_free(funcdef->name);

    for (ListNode *curr = funcdef->parms.head; curr; ) {
        ListNode *next = curr->next;

        TacNode *parm = CONTAINER_OF(curr, TacNode, list);
        tac_free(parm);

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
// Constructor for a TAC static variable declaration.
//
TacNode *tac_static_var(char *name, bool global, Type *type, List init, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_STATIC_VAR, loc);
    tac->static_var.name = safe_strdup(name);
    tac->static_var.global = global;
    tac->static_var.type = type;
    tac->static_var.init = init;
    return tac;
}

//
// Free a TAC static variable.
//
static void tac_static_var_free(TacStaticVar *var)
{
    safe_free(var->name);
}

//
// Constructor for a TAC return statement.
//
// The value, if it exists, must be a constant or a variable
//
TacNode *tac_return(TacNode *val, FileLine loc)
{
    ICE_ASSERT(val == NULL || (val->tag == TAC_CONST || val->tag == TAC_VAR));

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
    ICE_ASSERT(src->tag == TAC_CONST || src->tag == TAC_VAR);
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
    ICE_ASSERT(src->tag == TAC_CONST || src->tag == TAC_VAR);
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
    ICE_ASSERT(left->tag == TAC_CONST || left->tag == TAC_VAR);
    ICE_ASSERT(right->tag == TAC_CONST || right->tag == TAC_VAR);
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
// Construct a TAC constant.
//
TacNode *tac_const(Const cn, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_CONST, loc);
    tac->constant = cn;
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
// Constructor for a TAC sign extend operation.
//
TacNode *tac_sign_extend(TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_SIGN_EXTEND, loc);

    tac->sign_extend.src = src;
    tac->sign_extend.dst = dst;

    return tac;
}

//
// Constructor for a TAC zero extend operation.
//
TacNode *tac_zero_extend(TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_ZERO_EXTEND, loc);

    tac->zero_extend.src = src;
    tac->zero_extend.dst = dst;

    return tac;
}

//
// Constructor for a TAC truncate operation.
//
TacNode *tac_truncate(TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_TRUNCATE, loc);

    tac->truncate.src = src;
    tac->truncate.dst = dst;

    return tac;
}

//
// Constructor for a TAC double to int operation.
//
TacNode *tac_double_to_int(TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_DOUBLE_TO_INT, loc);

    tac->dbl_to_int.src = src;
    tac->dbl_to_int.dst = dst;

    return tac;
}

//
// Constructor for a TAC double to uint operation.
//
TacNode *tac_double_to_uint(TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_DOUBLE_TO_UINT, loc);

    tac->dbl_to_uint.src = src;
    tac->dbl_to_uint.dst = dst;

    return tac;
}

//
// Constructor for a TAC double to int operation.
//
TacNode *tac_int_to_double(TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_INT_TO_DOUBLE, loc);

    tac->int_to_dbl.src = src;
    tac->int_to_dbl.dst = dst;

    return tac;
}

//
// Constructor for a TAC double to int operation.
//
TacNode *tac_uint_to_double(TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_UINT_TO_DOUBLE, loc);

    tac->uint_to_dbl.src = src;
    tac->uint_to_dbl.dst = dst;

    return tac;
}

//
// Constructor for a TAC get address operation.
//
TacNode *tac_get_address(TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_GET_ADDRESS, loc);

    tac->get_address.src = src;
    tac->get_address.dst = dst;

    return tac;
}

//
// Constructor for a TAC load operation.
//
TacNode *tac_load(TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_LOAD, loc);

    tac->load.src = src;
    tac->load.dst = dst;

    return tac;
}

//
// Constructor for a TAC store operation.
//
TacNode *tac_store(TacNode *src, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_STORE, loc);

    tac->store.src = src;
    tac->store.dst = dst;

    return tac;
}

//
// Constructor for a TAC add to pointer operation.
//
TacNode *tac_add_ptr(TacNode *ptr, TacNode *index, size_t scale, TacNode *dst, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_ADDPTR, loc);

    tac->add_ptr.ptr = ptr;
    tac->add_ptr.index = index;
    tac->add_ptr.scale = scale;
    tac->add_ptr.dst = dst;

    return tac;
}

//
// Constructor for a TAC copy to offset operation.
//
TacNode *tac_copy_to_offset(TacNode *src, char *dst, int offset, FileLine loc)
{
    TacNode *tac = tac_alloc(TAC_COPY_TO_OFFSET, loc);

    tac->copy_to_offset.src = src;
    tac->copy_to_offset.dst = dst;
    tac->copy_to_offset.offset = offset;

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
// Free a TAC sign extend.
//
static void tac_sign_extend_free(TacSignExtend *sext)
{
    tac_free(sext->src);
    tac_free(sext->dst);
}

//
// Free a TAC zero extend.
//
static void tac_zero_extend_free(TacZeroExtend *zext)
{
    tac_free(zext->src);
    tac_free(zext->dst);
}

//
// Free a TAC truncate.
//
static void tac_truncate_free(TacTruncate *trunc)
{
    tac_free(trunc->src);
    tac_free(trunc->dst);
}

//
// Free a TAC double to int.
//
static void tac_double_to_int_free(TacDoubleToInt *dbl)
{
    tac_free(dbl->src);
    tac_free(dbl->dst);
}

//
// Free a TAC double to unsigned int.
//
static void tac_double_to_uint_free(TacDoubleToUInt *dbl)
{
    tac_free(dbl->src);
    tac_free(dbl->dst);
}

//
// Free a TAC int to double.
//
static void tac_int_to_double_free(TacIntToDouble *dbl)
{
    tac_free(dbl->src);
    tac_free(dbl->dst);
}

//
// Free a TAC unsigned int to double.
//
static void tac_uint_to_double_free(TacUIntToDouble *dbl)
{
    tac_free(dbl->src);
    tac_free(dbl->dst);
}

//
// Free a TAC get address operation.
//
static void tac_get_address_free(TacGetAddress *get_addr)
{
    tac_free(get_addr->src);
    tac_free(get_addr->dst);
}

//
// Free a TAC load operation.
//
static void tac_load_free(TacLoad *load)
{
    tac_free(load->src);
    tac_free(load->dst);
}

//
// Free a TAC store operation.
//
static void tac_store_free(TacStore *store)
{
    tac_free(store->src);
    tac_free(store->dst);
}

//
// Free a TAC add pointer operation.
//
static void tac_add_ptr_free(TacAddPtr *addptr)
{
    tac_free(addptr->ptr);
    tac_free(addptr->index);
    tac_free(addptr->dst);
}

//
// Free a TAC copy to offset operation.
//
static void tac_copy_to_offset_free(TacCopyToOffset *copy)
{
    tac_free(copy->src);
    safe_free(copy->dst);
}

//
// Clone and operand node. The operand must be of type
// TAC_VAR or TAC_CONST.
//
TacNode *tac_clone_operand(TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_VAR || tac->tag == TAC_CONST);

    switch (tac->tag) {
        case TAC_VAR:       return tac_var(tac->var.name, tac->loc);
        case TAC_CONST:     return tac_const(tac->constant, tac->loc);
    
        default:
            return tac_const(const_make_zero(), tac->loc);
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
            case TAC_STATIC_VAR:    tac_static_var_free(&tac->static_var); break;
            case TAC_CONST:         break;
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
            case TAC_SIGN_EXTEND:   tac_sign_extend_free(&tac->sign_extend); break;
            case TAC_ZERO_EXTEND:   tac_zero_extend_free(&tac->zero_extend); break;
            case TAC_TRUNCATE:      tac_truncate_free(&tac->truncate); break;
            case TAC_DOUBLE_TO_INT: tac_double_to_int_free(&tac->dbl_to_int); break;
            case TAC_DOUBLE_TO_UINT:tac_double_to_uint_free(&tac->dbl_to_uint); break;
            case TAC_INT_TO_DOUBLE: tac_int_to_double_free(&tac->int_to_dbl); break;
            case TAC_UINT_TO_DOUBLE:tac_uint_to_double_free(&tac->uint_to_dbl); break;
            case TAC_GET_ADDRESS:   tac_get_address_free(&tac->get_address); break;
            case TAC_LOAD:          tac_load_free(&tac->load); break;
            case TAC_STORE:         tac_store_free(&tac->store); break;
            case TAC_ADDPTR:        tac_add_ptr_free(&tac->add_ptr); break;
            case TAC_COPY_TO_OFFSET:tac_copy_to_offset_free(&tac->copy_to_offset); break;
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
    printf("%*sfunction(%s) ", tab, "", funcdef->name);
    if (funcdef->global) {
        printf("global ");
    }
    printf("(");

    for (ListNode *curr = funcdef->parms.head; curr; curr = curr->next) {
        TacNode *parm = CONTAINER_OF(curr, TacNode, list);
        ICE_ASSERT(parm->tag == TAC_VAR);
        printf("%s", parm->var.name);
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
// Print a TAC static variable declaration.
//
static void tac_print_static_var(TacStaticVar *var, int tab)
{
    char *desc = type_describe(var->type);
    printf("%*sstatic-var(%s %s) ", tab, "", desc, var->name);
    safe_free(desc);

    if (var->global) {
        printf("global ");
    }

    if (var->init.head) {
        printf("= [");

        for (ListNode *curr = var->init.head; curr; curr = curr->next) {
            StaticInitializer *init = CONTAINER_OF(curr, StaticInitializer, list);
            switch (init->tag) {
                case SI_ZERO: 
                    printf("zero[%zd]", init->bytes);
                    break;

                case SI_CONST:
                    switch (init->cval.tag) {
                        case CON_INTEGRAL:  printf("%lu", init->cval.intval.value); break;
                        case CON_FLOAT:     printf("%g", init->cval.floatval); break;
                    }
                    break;
            }

            if (curr->next) {
                printf(", ");
            }
        }

        printf("]");
    }

    printf("\n");
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
// Print a TAC constant.
//
static void tac_print_const(Const *constant, int tab)
{
    char *desc = const_describe(constant);
    printf("%*sconst(%s)\n", tab, "", desc);
    safe_free(desc);
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
// Print a TAC sign extend operation.
//
static void tac_print_sign_extend(TacSignExtend *sext, int tab, bool locs)
{
    printf("%*ssign-extend(\n", tab, "");
    tac_print_recurse(sext->src, tab + 2, locs);
    printf("%*s=>\n", tab, "");
    tac_print_recurse(sext->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC zero extend operation.
//
static void tac_print_zero_extend(TacZeroExtend *zext, int tab, bool locs)
{
    printf("%*szero-extend(\n", tab, "");
    tac_print_recurse(zext->src, tab + 2, locs);
    printf("%*s=>\n", tab, "");
    tac_print_recurse(zext->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC truncate operation.
//
static void tac_print_truncate(TacTruncate *trunc, int tab, bool locs)
{
    printf("%*struncate(\n", tab, "");
    tac_print_recurse(trunc->src, tab + 2, locs);
    printf("%*s=>\n", tab, "");
    tac_print_recurse(trunc->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC double to int operation.
//
static void tac_print_dbl_to_int(TacDoubleToInt *dbl, int tab, bool locs)
{
    printf("%*sdouble-to-int(\n", tab, "");
    tac_print_recurse(dbl->src, tab + 2, locs);
    printf("%*s=>\n", tab, "");
    tac_print_recurse(dbl->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC double to unsigned int operation.
//
static void tac_print_dbl_to_uint(TacDoubleToUInt *dbl, int tab, bool locs)
{
    printf("%*sdouble-to-uint(\n", tab, "");
    tac_print_recurse(dbl->src, tab + 2, locs);
    printf("%*s=>\n", tab, "");
    tac_print_recurse(dbl->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC int to double operation.
//
static void tac_print_int_to_dbl(TacIntToDouble *dbl, int tab, bool locs)
{
    printf("%*sint-to-double(\n", tab, "");
    tac_print_recurse(dbl->src, tab + 2, locs);
    printf("%*s=>\n", tab, "");
    tac_print_recurse(dbl->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC uint to double operation.
//
static void tac_print_uint_to_dbl(TacUIntToDouble *dbl, int tab, bool locs)
{
    printf("%*sint-to-double(\n", tab, "");
    tac_print_recurse(dbl->src, tab + 2, locs);
    printf("%*s=>\n", tab, "");
    tac_print_recurse(dbl->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC get address operation.
//
static void tac_print_get_address(TacGetAddress *get_addr, int tab, bool locs)
{
    printf("%*sget-address(\n", tab, "");
    tac_print_recurse(get_addr->src, tab + 2, locs);
    printf("%*s=>\n", tab, "");
    tac_print_recurse(get_addr->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC load operation.
//
static void tac_print_load(TacLoad *load, int tab, bool locs)
{
    printf("%*sload(\n", tab, "");
    tac_print_recurse(load->src, tab + 2, locs);
    printf("%*s=>\n", tab, "");
    tac_print_recurse(load->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC store operation.
//
static void tac_print_store(TacStore *store, int tab, bool locs)
{
    printf("%*sstore(\n", tab, "");
    tac_print_recurse(store->src, tab + 2, locs);
    printf("%*s=>\n", tab, "");
    tac_print_recurse(store->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC add to pointer operation.
//
static void tac_print_add_ptr(TacAddPtr *addptr, int tab, bool locs)
{
    printf("%*sadd-ptr(\n", tab, "");
    tac_print_recurse(addptr->ptr, tab + 2, locs);
    printf("%*s+\n", tab, "");
    tac_print_recurse(addptr->index, tab + 2, locs);
    printf("%*s* %zd =>\n", tab, "", addptr->scale);
    tac_print_recurse(addptr->dst, tab + 2, locs);
    printf("%*s)\n", tab, "");
}

//
// Print a TAC copy to offset operation.
//
static void tac_print_copy_to_offset(TacCopyToOffset *copy, int tab, bool locs)
{
    printf("%*scopy-to-offset(\n", tab, "");
    tac_print_recurse(copy->src, tab + 2, locs);
    printf("%*s=> %s at %d)\n", tab, "", copy->dst, copy->offset);
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
        case TAC_STATIC_VAR:    tac_print_static_var(&tac->static_var, tab); break;
        case TAC_RETURN:        tac_print_return(&tac->ret, tab, locs); break;
        case TAC_COPY:          tac_print_copy(&tac->copy, tab, locs); break;
        case TAC_JUMP:          tac_print_jump(&tac->jump, tab, locs); break;
        case TAC_JUMP_ZERO:     tac_print_jump_zero(&tac->jump_zero, tab, locs); break;
        case TAC_JUMP_NOT_ZERO: tac_print_jump_not_zero(&tac->jump_not_zero, tab, locs); break;
        case TAC_LABEL:         tac_print_label(&tac->label, tab); break;
        case TAC_UNARY:         tac_print_unary(&tac->unary, tab, locs); break;
        case TAC_BINARY:        tac_print_binary(&tac->binary, tab, locs); break;
        case TAC_CONST:         tac_print_const(&tac->constant, tab); break;
        case TAC_VAR:           tac_print_var(&tac->var, tab); break;
        case TAC_FUNCTION_CALL: tac_print_function_call(&tac->call, tab, locs); break;
        case TAC_SIGN_EXTEND:   tac_print_sign_extend(&tac->sign_extend, tab, locs); break;
        case TAC_ZERO_EXTEND:   tac_print_zero_extend(&tac->zero_extend, tab, locs); break;
        case TAC_TRUNCATE:      tac_print_truncate(&tac->truncate, tab, locs); break;
        case TAC_DOUBLE_TO_INT: tac_print_dbl_to_int(&tac->dbl_to_int, tab, locs); break;
        case TAC_DOUBLE_TO_UINT:tac_print_dbl_to_uint(&tac->dbl_to_uint, tab, locs); break;
        case TAC_INT_TO_DOUBLE: tac_print_int_to_dbl(&tac->int_to_dbl, tab, locs); break;
        case TAC_UINT_TO_DOUBLE:tac_print_uint_to_dbl(&tac->uint_to_dbl, tab, locs); break;
        case TAC_GET_ADDRESS:   tac_print_get_address(&tac->get_address, tab, locs); break;
        case TAC_LOAD:          tac_print_load(&tac->load, tab, locs); break;
        case TAC_STORE:         tac_print_store(&tac->store, tab, locs); break;
        case TAC_ADDPTR:        tac_print_add_ptr(&tac->add_ptr, tab, locs); break;
        case TAC_COPY_TO_OFFSET:tac_print_copy_to_offset(&tac->copy_to_offset, tab, locs); break;
    }
}

//
// Print a TAC tree, with location information if `locs` is true.
//
void tac_print(TacNode *tac, bool locs)
{
    tac_print_recurse(tac, 0, locs);
}
