#include "codegen.h"

#include "asm-ast.h"
#include "ice.h"
#include "list.h"
#include "safemem.h"
#include "symtab.h"
#include "temporary.h"

#include <ctype.h>

typedef struct {
    List code;
    List *statics;
    BackEndSymbolTable *bstab;
    SymbolTable *stab;
    char *neg_zero;
    char *dbl_to_uint_ub;
} CodegenState;

static Register int_arg_regs[] =
{
    REG_RDI,
    REG_RSI,
    REG_RDX,
    REG_RCX,
    REG_R8,
    REG_R9,
};
static const int int_arg_reg_count = sizeof(int_arg_regs) / sizeof(int_arg_regs[0]);

static Register float_arg_regs[] =
{
    REG_XMM0,
    REG_XMM1,
    REG_XMM2,
    REG_XMM3,
    REG_XMM4,
    REG_XMM5,
    REG_XMM6,
    REG_XMM7,
};
static const int float_arg_reg_count = sizeof(float_arg_regs) / sizeof(float_arg_regs[0]);

#define DBL_TO_UINT_UPPER_FLT 9223372036854775808.0
#define DBL_TO_UINT_UPPER_INT 9223372036854775808lu

//
// Create a nested state.
//
static CodegenState nested_state(CodegenState *outer)
{
    CodegenState nested = *outer;
    return nested;
}

//
// Return true if the given TAC operand is unsigned.
//
// The TAC node must be either a constant or a variable.
//
static bool codegen_operand_unsigned(CodegenState *state, TacNode *tac)
{
    if (tac->tag == TAC_CONST) {
        return const_unsigned(&tac->constant);
    }
    
    if (tac->tag == TAC_VAR) {
        Symbol *sym = stab_lookup(state->stab, tac->var.name);
        return type_unsigned(sym->type);
    }

    ICE_ASSERT(((void)"invalid TAC tag in codegen_operand_unsigned", false));
    return false;
}

// Return true if the given TAC operand is a float.
//
// The TAC node must either be a variable or a constant.
//
static bool codegen_operand_float(CodegenState *state, TacNode *tac)
{
    //
    // If it's a constant, the type is specified.
    //
    if (tac->tag == TAC_CONST) {
        return tac->constant.tag == CON_FLOAT;
    }

    //
    // If it's a variable, look it up in the symbol 
    // table to see what type it is.
    //
    if (tac->tag == TAC_VAR) {
        Symbol *sym = stab_lookup(state->stab, tac->var.name);
        return sym->type->tag == TT_DOUBLE;
    }

    ICE_ASSERT(((void)"TAC node in codegen_operand_float was not an operand.", false));
    return false;
}

//
// Push an assembly instruction onto a code list.
//
static void codegen_push_instr(CodegenState *state, AsmNode *instr)
{
    list_push_back(&state->code, &instr->list);
}

//
// Push a static variable definition onto the statics list.
//
static void codegen_push_static(CodegenState *state, AsmNode *instr)
{
    list_push_back(state->statics, &instr->list);
}

static AsmOperand *codegen_expression(CodegenState *state, TacNode *tac);

//
// Convert a type system type to the proper assembly type.
//
static AsmType *codegen_type_to_asmtype(Type *type)
{
    ICE_ASSERT(type != NULL);

    switch (type->tag) {
        case TT_INT:        return asmtype_long();
        case TT_UINT:       return asmtype_long();
        case TT_LONG:       return asmtype_quad();
        case TT_ULONG:      return asmtype_quad();
        case TT_DOUBLE:     return asmtype_double();
        case TT_FUNC:       ICE_ASSERT(((void)"function type found in codegen_type_to_asmtype.", false));
        case TT_POINTER:    return asmtype_quad();
    }
    
    ICE_ASSERT(false);
    return asmtype_long();
}

//
// Convert a TAC operand node to the proper assembly type.
//
// The TAC node must either be a variable or a constant.
//
static AsmType *codegen_tac_to_asmtype(CodegenState *state, TacNode *tac)
{
    //
    // If it's a constant, the size is specified.
    //
    if (tac->tag == TAC_CONST) {
        switch (tac->constant.tag) {
            case CON_INTEGRAL:
                if (tac->constant.intval.size == CIS_LONG) {
                    return asmtype_quad();
                } else {
                    return asmtype_long();
                }
                break;

            case CON_FLOAT: 
                return asmtype_double();
        }
    }

    //
    // If it's a variable, look it up in the symbol 
    // table to see what size it is.
    //
    if (tac->tag == TAC_VAR) {
        Symbol *sym = stab_lookup(state->stab, tac->var.name);
        return codegen_type_to_asmtype(sym->type);
    }

    ICE_ASSERT(((void)"TAC node in codegen_tac_to_asmtype was not an operand.", false));
    return asmtype_long();
}

//
//
// Return the alignment for a static variable.
//
static int codegen_align(Type *type)
{
    int align = 1;
    AsmType *at = codegen_type_to_asmtype(type);

    switch (at->tag) {
        case AT_LONGWORD:   align = 4; break;
        case AT_QUADWORD:   align = 8; break;
        case AT_DOUBLE:     align = 8; break;
    }

    asmtype_free(at);

    return align;
}

//
// Create a labeled floating point literal and return the allocated label.
//
static char *codegen_float_literal(CodegenState *state, double floatval, int align, FileLine loc)
{
    char *val = saprintf("float_%g", floatval);
    for (char *p = val; *p; p++) {
        //
        // Make value safe as a label
        //
        if (!(isalpha(*p) || isdigit(*p) || *p == '_' || *p == '.')) {
            *p = '_';
        }
    }

    char *label = tmp_name(val);
    safe_free(val);

    codegen_push_static(state, asm_static_const(label, align, const_make_double(floatval), loc));

    BackEndSymbol *sym = bstab_lookup(state->bstab, label);
    sym->tag = BST_OBJECT;
    sym->object.type = asmtype_double();
    sym->object.is_static = true;
    sym->object.is_literal = true;

    return label;
}

//
// Generate code for a constant.
//
static AsmOperand *codegen_constant(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_CONST);
    
    if (tac->constant.tag == CON_FLOAT) {
        char *label = codegen_float_literal(state, tac->constant.floatval, 8, tac->loc);
        return aoper_data(label);
    }

    return aoper_imm(tac->constant.intval.value);
}

//
// Generate code for a variable reference.
//
static AsmOperand *codegen_var(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_VAR);
    return aoper_pseudoreg(tac->var.name);
}

//
// Generate code for an expression. Code to generate the value
// will be appended to state->code, and an unreferenced operand
// node containing the result location will be returned.
//
static AsmOperand *codegen_expression(CodegenState *state, TacNode *tac)
{
    switch (tac->tag) {
        case TAC_CONST:     return codegen_constant(state, tac);
        case TAC_VAR:       return codegen_var(state, tac);

        default:
            ICE_ASSERT(("invalid TAC node in codegen_expression"));
    }

    //
    // never reached
    //
    return NULL;
}

//
// Generate code for a unary not operator.
//
static void codegen_unary_not(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_UNARY);
    ICE_ASSERT(tac->unary.op == UOP_LOGNOT);

    AsmOperand *src = codegen_expression(state, tac->unary.src);
    AsmOperand *dst = codegen_expression(state, tac->unary.dst);

    AsmType *srctype = codegen_tac_to_asmtype(state, tac->unary.src);
    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->unary.dst);

    if (srctype->tag == AT_DOUBLE) {
        codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_reg(REG_XMM13), aoper_reg(REG_XMM13), asmtype_double(), tac->loc));
        codegen_push_instr(state, asm_cmp(aoper_reg(REG_XMM13), src, srctype, tac->loc));
    } else {
        codegen_push_instr(state, asm_cmp(aoper_imm(0), src, srctype, tac->loc));
    }

    codegen_push_instr(state, asm_mov(aoper_imm(0), dst, dsttype, tac->loc));
    codegen_push_instr(state, asm_setcc(aoper_clone(dst), ACC_E, tac->loc));
}

//
// Generate code for a unary negation on floating point.
//
static void codegen_unary_neg_float(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_UNARY);
    ICE_ASSERT(tac->unary.op == UOP_MINUS);

    AsmOperand *src = codegen_expression(state, tac->unary.src);
    AsmOperand *dst = codegen_expression(state, tac->unary.dst);

    if (state->neg_zero == NULL) {
        state->neg_zero = codegen_float_literal(state, -0.0, 16, tac->loc);
    }

    codegen_push_instr(state, asm_mov(src, dst, asmtype_double(), tac->loc));
    codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_data(state->neg_zero), aoper_clone(dst), asmtype_double(), tac->loc));
}

//
// Generate code for a unary operator.
//
static void codegen_unary(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_UNARY);

    if (tac->unary.op == UOP_LOGNOT) {
        codegen_unary_not(state, tac);
        return;
    }

    if (tac->unary.op == UOP_MINUS && codegen_operand_float(state, tac->unary.dst)) {
        codegen_unary_neg_float(state, tac);
        return;
    }

    AsmOperand *src = codegen_expression(state, tac->unary.src);
    AsmOperand *dst = codegen_expression(state, tac->unary.dst);

    AsmType *srctype = codegen_tac_to_asmtype(state, tac->unary.src);
    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->unary.dst);
    codegen_push_instr(state, asm_mov(src, dst, srctype, tac->loc));
    codegen_push_instr(state, asm_unary(tac->unary.op, aoper_clone(dst), dsttype, tac->loc));
}

//
// Generate code for a relational operator.
//
static void codegen_relational(CodegenState *state, TacNode *tac)
{
    AsmConditionCode cc = ACC_E;

    bool is_unsigned = codegen_operand_unsigned(state, tac->binary.left);
    bool is_float = codegen_operand_float(state, tac->binary.left);

    if (is_unsigned || is_float) {
        switch (tac->binary.op) {
            case BOP_EQUALITY:      cc = ACC_E; break;
            case BOP_NOTEQUAL:      cc = ACC_NE; break;
            case BOP_LESSTHAN:      cc = ACC_B; break;
            case BOP_GREATERTHAN:   cc = ACC_A; break;
            case BOP_LESSEQUAL:     cc = ACC_BE; break;
            case BOP_GREATEREQUAL:  cc = ACC_AE; break;
            
            default:
                ICE_ASSERT(((void)"invalid binary op in codegen_relational", false));
        }
    } else {
        switch (tac->binary.op) {
            case BOP_EQUALITY:      cc = ACC_E; break;
            case BOP_NOTEQUAL:      cc = ACC_NE; break;
            case BOP_LESSTHAN:      cc = ACC_L; break;
            case BOP_GREATERTHAN:   cc = ACC_G; break;
            case BOP_LESSEQUAL:     cc = ACC_LE; break;
            case BOP_GREATEREQUAL:  cc = ACC_GE; break;
            
            default:
                ICE_ASSERT(((void)"invalid binary op in codegen_relational", false));
        }
    }

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    AsmType *cmptype = codegen_tac_to_asmtype(state, tac->binary.left);
    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->binary.dst);

    codegen_push_instr(state, asm_cmp(right, left, cmptype, tac->loc));
    codegen_push_instr(state, asm_mov(aoper_imm(0), dst, dsttype, tac->loc));
    codegen_push_instr(state, asm_setcc(aoper_clone(dst), cc, tac->loc));
}

//
// Generate code for an integer divide (or modulo) operation.
//
static void codegen_int_div(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_BINARY);
    ICE_ASSERT(tac->binary.op == BOP_DIVIDE || tac->binary.op == BOP_MODULO);

    bool is_unsigned = codegen_operand_unsigned(state, tac->binary.left);
    AsmType *lefttype = codegen_tac_to_asmtype(state, tac->binary.left);

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    codegen_push_instr(state, asm_mov(left, aoper_reg(REG_RAX), lefttype, tac->loc));
    if (is_unsigned) {
        lefttype = asmtype_clone(lefttype);
        codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_reg(REG_RDX), aoper_reg(REG_RDX), lefttype, tac->loc));
        lefttype = asmtype_clone(lefttype);
        codegen_push_instr(state, asm_div(right, lefttype, tac->loc));
    } else {
        lefttype = asmtype_clone(lefttype);
        codegen_push_instr(state, asm_cdq(lefttype, tac->loc));
        lefttype = asmtype_clone(lefttype);
        codegen_push_instr(state, asm_idiv(right, lefttype, tac->loc));
    }

    //
    // div/idiv return quotient in AX, remainder in DX
    //
    Register dstreg = tac->binary.op == BOP_DIVIDE ? REG_RAX : REG_RDX;
    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->binary.left);
    codegen_push_instr(state, asm_mov(aoper_reg(dstreg), dst, dsttype, tac->loc));
}

//
// Generate code for a floating point divide.
//
static void codegen_float_div(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_BINARY);
    ICE_ASSERT(tac->binary.op == BOP_DIVIDE);   // NOTE: no modulo in floating point.

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    codegen_push_instr(state, asm_mov(left, dst, asmtype_double(), tac->loc));
    codegen_push_instr(state, asm_binary(BOP_DIVDBL, right, aoper_clone(dst), asmtype_double(), tac->loc));
}

//
// Generate code for a binary operator.
//
static void codegen_binary(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_BINARY);

    if (bop_is_relational(tac->binary.op)) {
        codegen_relational(state, tac);
        return;
    }

    if (tac->binary.op == BOP_DIVIDE || tac->binary.op == BOP_MODULO) {
        if (codegen_operand_float(state, tac->binary.dst)) {
            codegen_float_div(state, tac);
        } else {
            codegen_int_div(state, tac);
        }
        return;
    }

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->binary.left);
    codegen_push_instr(state, asm_mov(left, dst, dsttype, tac->loc));
    dst = aoper_clone(dst);
    dsttype = asmtype_clone(dsttype);
    codegen_push_instr(state, asm_binary(tac->binary.op, right, dst, dsttype, tac->loc));
}

//
// Generate code for a return instruction.
//
static void codegen_return(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_RETURN);

    AsmOperand *retval = codegen_expression(state, tac->ret.val);

    AsmType *rettype = codegen_tac_to_asmtype(state, tac->ret.val);

    if (rettype->tag == AT_DOUBLE) {
        codegen_push_instr(state, asm_mov(retval, aoper_reg(REG_XMM0), rettype, tac->loc));
    } else {
        codegen_push_instr(state, asm_mov(retval, aoper_reg(REG_RAX), rettype, tac->loc));
    }
    codegen_push_instr(state, asm_ret(tac->loc));
}

//
// Generate code for a jump instruction.
//
static void codegen_jump(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_JUMP);

    codegen_push_instr(state, asm_jump(tac->jump.target, tac->loc));
}

//
// Generate code for a jump-on-zero instruction.
//
static void codegen_jump_zero(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_JUMP_ZERO);

    AsmOperand *cond = codegen_expression(state, tac->jump_zero.condition);
    AsmType *condtype = codegen_tac_to_asmtype(state, tac->jump_zero.condition);

    if (condtype->tag == AT_DOUBLE) {
        codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_reg(REG_XMM13), aoper_reg(REG_XMM13), condtype, tac->loc));
        codegen_push_instr(state, asm_cmp(cond, aoper_reg(REG_XMM13), asmtype_clone(condtype), tac->loc));
    } else {
        codegen_push_instr(state, asm_cmp(aoper_imm(0), cond, condtype, tac->loc));
    }
    codegen_push_instr(state, asm_jumpcc(tac->jump_zero.target, ACC_E, tac->loc));
}

//
// Generate code for a jump-on-not-zero instruction.
//
static void codegen_jump_not_zero(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_JUMP_NOT_ZERO);

    AsmOperand *cond = codegen_expression(state, tac->jump_not_zero.condition);
    AsmType *condtype = codegen_tac_to_asmtype(state, tac->jump_not_zero.condition);

    if (condtype->tag == AT_DOUBLE) {
        codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_reg(REG_XMM13), aoper_reg(REG_XMM13), condtype, tac->loc));
        codegen_push_instr(state, asm_cmp(cond, aoper_reg(REG_XMM13), asmtype_clone(condtype), tac->loc));
    } else {
        codegen_push_instr(state, asm_cmp(aoper_imm(0), cond, condtype, tac->loc));
    }
    codegen_push_instr(state, asm_jumpcc(tac->jump_zero.target, ACC_NE, tac->loc));
}

//
// Generate code for a copy instruction.
//
static void codegen_copy(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_COPY);

    AsmOperand *src = codegen_expression(state, tac->copy.src);
    AsmOperand *dst = codegen_expression(state, tac->copy.dst);
    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->copy.dst);

    codegen_push_instr(state, asm_mov(src, dst, dsttype, tac->loc));
}

//
// Generate code for a label.
//
static void codegen_label(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_LABEL);

    codegen_push_instr(state, asm_label(tac->label.name, tac->loc));
}

//
// Given a list of TAC operand nodes, partition the list into new lists based on
// parameter passing conventions.
//
// - First 6 integer operands go into integer registers.
// - First 8 float operands go into XMM registers.
// - Remaining operands go onto the stack.
//
// The original list will be destroyed.
//
static void codegen_classify_parameters(CodegenState *state, List *nodes, List *ints, List *floats, List *stack)
{
    int nints = 0;
    int nfloats = 0;

    list_clear(ints);
    list_clear(floats);
    list_clear(stack);

    ListNode *next = NULL;
    for (ListNode *curr = nodes->head; curr; curr = next) {
        next = curr->next;

        TacNode *parm = CONTAINER_OF(curr, TacNode, list);

        if (nfloats < float_arg_reg_count && codegen_operand_float(state, parm)) {
            list_push_back(floats, curr);
            nfloats++;
            continue;
        }

        if (nints < int_arg_reg_count && !codegen_operand_float(state, parm)) {
            list_push_back(ints, curr);
            nints++;
            continue;
        }

        list_push_back(stack, curr);
    }
}

//
// Generate code for a function call.
//
// The stack pointer on entry is guaranteed to be 16-byte aligned; it must
// maintain this alignment at the call point.
//
// The calling convention takes the first 6 integer arguments in registers;
// following arguments are pushed in reverse order on the stack. The caller
// is responsible for stack cleanup.
//
static void codegen_function_call(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_FUNCTION_CALL);
    TacFunctionCall *call = &tac->call;

    List ints, floats, stack;

    codegen_classify_parameters(state, &call->args, &ints, &floats, &stack);

    int args_on_stack = list_count(&stack);

    //
    // We push 8-byte arguments on the stack, but the stack pointer must stay 
    // 16-byte aligned; if there are an odd number of stack arguments, insert 
    // 8 bytes of padding.
    //
    int stack_padding = (args_on_stack & 1) ? 8 : 0;
    
    if (stack_padding) {
        codegen_push_instr(state, asm_stack_reserve(stack_padding, tac->loc));
    }

    //
    // Load integer register arguments.
    //
    ListNode *curr = ints.head;
    for (int i = 0; curr; i++, curr = curr->next) {
        Register arg_reg = int_arg_regs[i];
        TacNode *tac_arg = CONTAINER_OF(curr, TacNode, list);
        AsmOperand *arg = codegen_expression(state, tac_arg);
        AsmType *argtype = codegen_tac_to_asmtype(state, tac_arg);
        codegen_push_instr(state, asm_mov(arg, aoper_reg(arg_reg), argtype, tac->loc));
    }

    //
    // Load floating point register arguments.
    //
    curr = floats.head;
    for (int i = 0; curr; i++, curr = curr->next) {
        Register arg_reg = float_arg_regs[i];
        TacNode *tac_arg = CONTAINER_OF(curr, TacNode, list);
        AsmOperand *arg = codegen_expression(state, tac_arg);
        AsmType *argtype = codegen_tac_to_asmtype(state, tac_arg);
        codegen_push_instr(state, asm_mov(arg, aoper_reg(arg_reg), argtype, tac->loc));
    }

    //
    // Push stack args.
    //
    list_reverse(&stack);

    curr = stack.head;
    for (int i = 0; i < args_on_stack; i++, curr = curr->next) {
        TacNode *tac_arg = CONTAINER_OF(curr, TacNode, list);
        AsmOperand *arg = codegen_expression(state, tac_arg);
        AsmType *argtype = codegen_tac_to_asmtype(state, tac_arg);
        if (argtype->tag == AT_DOUBLE) {
            codegen_push_instr(state, asm_push(arg, tac->loc));
        } else {
            codegen_push_instr(state, asm_mov(arg, aoper_reg(REG_RAX), argtype, tac->loc));
            codegen_push_instr(state, asm_push(aoper_reg(REG_RAX), tac->loc));
        }
    }

    //
    // Do the call
    //
    codegen_push_instr(state, asm_call(call->name, tac->loc));

    //
    // Clean up
    //
    int bytes_to_free = stack_padding + 8 * args_on_stack;
    if (bytes_to_free) {
        codegen_push_instr(state, asm_stack_free((bytes_to_free), tac->loc));
    }

    //
    // Return value, if any, goes in RAX or XMM0.
    //
    AsmOperand *result = codegen_expression(state, call->dst);
    AsmType *restype = codegen_tac_to_asmtype(state, call->dst);
    
    if (restype->tag == AT_DOUBLE) {
        codegen_push_instr(state, asm_mov(aoper_reg(REG_XMM0), result, restype, tac->loc));
    } else {
        codegen_push_instr(state, asm_mov(aoper_reg(REG_RAX), result, restype, tac->loc));
    }
}

//
// Geneate code for a static variable.
//
static void codegen_static_var(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_STATIC_VAR);

    codegen_push_static(state, 
        asm_static_var(
            decl->static_var.name, 
            decl->static_var.global,
            codegen_align(decl->static_var.type),
            decl->static_var.init, 
            decl->loc));
}

//
// Generate code for a sign extend operation.
//
static void codegen_sign_extend(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_SIGN_EXTEND);

    codegen_push_instr(state,
        asm_movsx(
            codegen_expression(state, decl->sign_extend.src),
            codegen_expression(state, decl->sign_extend.dst),
            decl->loc)
    );
}

//
// Generate code for a zero extend operation.
//
static void codegen_zero_extend(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_ZERO_EXTEND);

    codegen_push_instr(state,
        asm_movzx(
            codegen_expression(state, decl->zero_extend.src),
            codegen_expression(state, decl->zero_extend.dst),
            decl->loc)
    );
}

//
// Generate code for a truncate operation.
//
static void codegen_truncate(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_TRUNCATE);

    codegen_push_instr(state,
        asm_mov(
            codegen_expression(state, decl->truncate.src),
            codegen_expression(state, decl->truncate.dst),
            asmtype_long(),
            decl->loc)
    );
}

//
// Generate code to convert a double to an integer.
//
static void codegen_dbl_to_int(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_DOUBLE_TO_INT); 

    AsmOperand *src = codegen_expression(state, tac->dbl_to_int.src);
    AsmOperand *dst = codegen_expression(state, tac->dbl_to_int.dst);
    AsmType *type = codegen_tac_to_asmtype(state, tac->dbl_to_int.dst);

    codegen_push_instr(state,
        asm_cvttsd2si(src, dst, type, tac->loc)
    );
}

//
// Generate code to convert a double to an unsigned integer.
//
static void codegen_dbl_to_uint(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_DOUBLE_TO_UINT); 

    AsmOperand *src = codegen_expression(state, tac->dbl_to_uint.src);
    AsmOperand *dst = codegen_expression(state, tac->dbl_to_uint.dst);
    AsmType *type = codegen_tac_to_asmtype(state, tac->dbl_to_uint.dst);

    if (type->tag == AT_LONGWORD) {
        codegen_push_instr(state, asm_cvttsd2si(aoper_clone(src), aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_mov(aoper_reg(REG_RAX), aoper_clone(dst), asmtype_long(), tac->loc));
    } else {
        if (state->dbl_to_uint_ub == NULL) {
            state->dbl_to_uint_ub = codegen_float_literal(state, DBL_TO_UINT_UPPER_FLT, 16, tac->loc);
        }

        char *label1 = tmp_name("label");
        char *label2 = tmp_name("label");

        //
        // This algorithm avoids rounding error in the conversion by adjusting the
        // value to convert if it's over the max that can be represented in a signed long.
        //
        codegen_push_instr(state, asm_cmp(aoper_clone(src), aoper_data(state->dbl_to_uint_ub), asmtype_double(), tac->loc));
        codegen_push_instr(state, asm_jumpcc(label1, ACC_AE, tac->loc));
        codegen_push_instr(state, asm_cvttsd2si(aoper_clone(src), aoper_clone(dst), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_jump(label2, tac->loc));
        codegen_push_instr(state, asm_label(label1, tac->loc));
        codegen_push_instr(state, asm_mov(aoper_clone(src), aoper_reg(REG_XMM13), asmtype_double(), tac->loc));
        codegen_push_instr(state, asm_binary(BOP_SUBTRACT, aoper_data(state->dbl_to_uint_ub), aoper_reg(REG_XMM13), asmtype_double(), tac->loc));
        codegen_push_instr(state, asm_cvttsd2si(aoper_reg(REG_XMM13), aoper_clone(dst), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_mov(aoper_imm(DBL_TO_UINT_UPPER_INT), aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_binary(BOP_ADD, aoper_reg(REG_RAX), aoper_clone(dst), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_label(label2, tac->loc));
        
        safe_free(label1);
        safe_free(label2);
    }

    asmtype_free(type);
    aoper_free(src);
    aoper_free(dst);
}

//
// Generate code to convert an integer to a double.
//
static void codegen_int_to_dbl(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_INT_TO_DOUBLE); 

    AsmOperand *src = codegen_expression(state, tac->int_to_dbl.src);
    AsmOperand *dst = codegen_expression(state, tac->int_to_dbl.dst);
    AsmType *type = codegen_tac_to_asmtype(state, tac->int_to_dbl.src);

    codegen_push_instr(state,
        asm_cvtsi2sd(src, dst, type, tac->loc)
    );
}

//
// Generate code to convert an unsigned integer to a double.
//
static void codegen_uint_to_dbl(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_UINT_TO_DOUBLE); 

    AsmOperand *src = codegen_expression(state, tac->uint_to_dbl.src);
    AsmOperand *dst = codegen_expression(state, tac->uint_to_dbl.dst);
    AsmType *type = codegen_tac_to_asmtype(state, tac->uint_to_dbl.src);

    if (type->tag == AT_LONGWORD) {
        codegen_push_instr(state, asm_movzx(aoper_clone(src), aoper_reg(REG_RAX), tac->loc));
        codegen_push_instr(state, asm_cvtsi2sd(aoper_reg(REG_RAX), aoper_clone(dst), asmtype_quad(), tac->loc));
    } else {
        char *label1 = tmp_name("label");
        char *label2 = tmp_name("label");

        codegen_push_instr(state, asm_cmp(aoper_imm(0), aoper_clone(src), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_jumpcc(label1, ACC_L, tac->loc));
        codegen_push_instr(state, asm_cvtsi2sd(aoper_clone(src), aoper_clone(dst), asmtype_clone(type), tac->loc));
        codegen_push_instr(state, asm_jump(label2, tac->loc));
        codegen_push_instr(state, asm_label(label1, tac->loc));
        codegen_push_instr(state, asm_mov(aoper_clone(src), aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_mov(aoper_reg(REG_RAX), aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_unary(UOP_SHR, aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_binary(BOP_BITAND, aoper_imm(1), aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_binary(BOP_BITOR, aoper_reg(REG_RAX), aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_cvtsi2sd(aoper_reg(REG_RDX), aoper_clone(dst), asmtype_clone(type), tac->loc));
        codegen_push_instr(state, asm_binary(BOP_ADD, aoper_clone(dst), aoper_clone(dst), asmtype_double(), tac->loc));
        codegen_push_instr(state, asm_label(label2, tac->loc));
        
        safe_free(label1);
        safe_free(label2);
    }

    asmtype_free(type);
    aoper_free(src);
    aoper_free(dst);
}

//
// Generate code for a get-address operation.
//
static void codegen_get_address(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_GET_ADDRESS);

    AsmOperand *src = codegen_expression(state, tac->get_address.src);
    AsmOperand *dst = codegen_expression(state, tac->get_address.dst);
    codegen_push_instr(state, asm_lea(src, dst, tac->loc));
}

//
// Generate code for a load operation.
// 
static void codegen_load(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_LOAD);

    AsmOperand *src = codegen_expression(state, tac->load.src);
    AsmOperand *dst = codegen_expression(state, tac->load.dst);
    AsmType *type = codegen_tac_to_asmtype(state, tac->load.dst);

    codegen_push_instr(state, asm_mov(src, aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
    codegen_push_instr(state, asm_mov(aoper_memory(REG_RDX, 0), dst, type, tac->loc));
}

//
// Generate code for a store operation.
// 
static void codegen_store(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_STORE);

    AsmOperand *src = codegen_expression(state, tac->store.src);
    AsmOperand *dst = codegen_expression(state, tac->store.dst);
    AsmType *type = codegen_tac_to_asmtype(state, tac->store.src);

    codegen_push_instr(state, asm_mov(dst, aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
    codegen_push_instr(state, asm_mov(src, aoper_memory(REG_RDX, 0), type, tac->loc));
}

//
// Generate code for a single instruction.
//
static void codegen_single(CodegenState *state, TacNode *tac)
{
    switch (tac->tag) {
        case TAC_UNARY:             codegen_unary(state, tac); break;
        case TAC_BINARY:            codegen_binary(state, tac); break;
        case TAC_RETURN:            codegen_return(state, tac); break;
        case TAC_JUMP:              codegen_jump(state, tac); break;
        case TAC_JUMP_ZERO:         codegen_jump_zero(state, tac); break;
        case TAC_JUMP_NOT_ZERO:     codegen_jump_not_zero(state, tac); break;
        case TAC_COPY:              codegen_copy(state, tac); break;
        case TAC_LABEL:             codegen_label(state, tac); break;
        case TAC_FUNCTION_CALL:     codegen_function_call(state, tac); break;

        case TAC_STATIC_VAR:        codegen_static_var(state, tac); break; 
        case TAC_SIGN_EXTEND:       codegen_sign_extend(state, tac); break;
        case TAC_ZERO_EXTEND:       codegen_zero_extend(state, tac); break;
        case TAC_TRUNCATE:          codegen_truncate(state, tac); break;

        case TAC_DOUBLE_TO_INT:     codegen_dbl_to_int(state, tac); break;
        case TAC_DOUBLE_TO_UINT:    codegen_dbl_to_uint(state, tac); break;
        case TAC_INT_TO_DOUBLE:     codegen_int_to_dbl(state, tac); break;
        case TAC_UINT_TO_DOUBLE:    codegen_uint_to_dbl(state, tac); break;

        case TAC_GET_ADDRESS:       codegen_get_address(state, tac); break;
        case TAC_LOAD:              codegen_load(state, tac); break;
        case TAC_STORE:             codegen_store(state, tac); break;

        case TAC_PROGRAM:           break;
        case TAC_CONST:             break;
        case TAC_VAR:               break;
        case TAC_FUNCDEF:           break;
    }
}

//
// Generate code for a function.
//
static void codegen_funcdef(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_FUNCDEF);
    TacFuncDef *func = &tac->funcdef;

    CodegenState funcstate = nested_state(state);
    list_clear(&funcstate.code);

    List ints;
    List floats;
    List stack;

    codegen_classify_parameters(state, &func->parms, &ints, &floats, &stack);

    //
    // Move parameters into local pseudoregisters.
    //

    //
    // Integer register based parameters.
    //
    ListNode *curr = ints.head;

    for (int i = 0; curr; i++, curr = curr->next) {
        TacNode *param_node = CONTAINER_OF(curr, TacNode, list);
        ICE_ASSERT(param_node->tag == TAC_VAR);
        TacVar *param = &param_node->var;

        Symbol *sym = stab_lookup(state->stab, param->name);

        AsmType *at = codegen_type_to_asmtype(sym->type);
        codegen_push_instr(
            &funcstate,
            asm_mov(
                aoper_reg(int_arg_regs[i]),
                aoper_pseudoreg(param->name),
                at,
                tac->loc
            )
        );
    }

    //
    // Float register based parameters.
    //
    curr = floats.head;
    for (int i = 0; curr; i++, curr = curr->next) {
        TacNode *param_node = CONTAINER_OF(curr, TacNode, list);
        ICE_ASSERT(param_node->tag == TAC_VAR);
        TacVar *param = &param_node->var;

        Symbol *sym = stab_lookup(state->stab, param->name);

        AsmType *at = codegen_type_to_asmtype(sym->type);
        codegen_push_instr(
            &funcstate,
            asm_mov(
                aoper_reg(float_arg_regs[i]),
                aoper_pseudoreg(param->name),
                at,
                tac->loc
            )
        );
    }

    //
    // Stack based parameters.
    //
    int offset = 16;
    curr = stack.head;
    for (int i = 0; curr; i++, curr = curr->next) {
        TacNode *param_node = CONTAINER_OF(curr, TacNode, list);
        ICE_ASSERT(param_node->tag == TAC_VAR);
        TacVar *param = &param_node->var;

        Symbol *sym = stab_lookup(state->stab, param->name);

        AsmType *at = codegen_type_to_asmtype(sym->type);
        codegen_push_instr(
            &funcstate,
            asm_mov(
                aoper_memory(REG_RBP, offset),
                aoper_pseudoreg(param->name),
                at,
                tac->loc
            )
        );

        offset += 8;
    }

    //
    // Statements.
    //
    for (ListNode *curr = tac->funcdef.body.head; curr; curr = curr->next) {
        TacNode *stmt = CONTAINER_OF(curr, TacNode, list);
        codegen_single(&funcstate, stmt);
    }

    codegen_push_instr(state, asm_func(tac->funcdef.name, funcstate.code, func->global, tac->loc));
}

//
// Generate code from the AST.
//
AsmNode *codegen(TacNode *tac, SymbolTable *stab, BackEndSymbolTable *bstab)
{
    ICE_ASSERT(tac->tag == TAC_PROGRAM);

    List statics;
    list_clear(&statics);

    CodegenState state;
    list_clear(&state.code);
    state.statics = &statics;
    state.stab = stab;
    state.bstab = bstab;
    state.neg_zero = NULL;
    state.dbl_to_uint_ub = NULL;

    for (ListNode *curr = tac->prog.decls.head; curr; curr = curr->next) {
        TacNode *decl = CONTAINER_OF(curr, TacNode, list);

        switch (decl->tag) {
            case TAC_FUNCDEF: codegen_funcdef(&state, decl); break;
            case TAC_STATIC_VAR: codegen_static_var(&state, decl); break;

            default: ICE_ASSERT(((void)"unexpected tag for top-level TAC node", false)); 
        }
    }

    list_append(&state.code, state.statics->head);

    AsmNode *prog = asm_prog(state.code, tac->loc);

    safe_free(state.dbl_to_uint_ub);
    safe_free(state.neg_zero);

    return prog;
}

//
// Convert a function symbol to a back end symbol.
//
void codegen_func_sym_to_backsym(SymFunction *func, BackEndSymbol *bsym)
{
    bsym->tag = BST_FUNCTION;
    bsym->func.is_defined = func->defined; 
}

//
// Convert a static variable symbol to a back end symbol.
//
void codegen_static_sym_to_backsym(Symbol *sym, BackEndSymbol *bsym)
{
    ICE_ASSERT(sym->tag == ST_STATIC_VAR);

    bsym->tag = BST_OBJECT;
    bsym->object.type = codegen_type_to_asmtype(sym->type);
    bsym->object.is_static = true;
}

//
// Convert a local variable symbol to a back end symbol.
//
void codegen_local_sym_to_backsym(Symbol *sym, BackEndSymbol *bsym)
{
    ICE_ASSERT(sym->tag == ST_LOCAL_VAR);

    bsym->tag = BST_OBJECT;
    bsym->object.type = codegen_type_to_asmtype(sym->type);
    bsym->object.is_static = false;
}

//
// Allocate a back end symbol table an populate it from the front end
// symbol table.
// 
void codegen_sym_to_backsym(SymbolTable *stab, BackEndSymbolTable *bstab)
{
    HashIterator iter;

    for (HashNode *curr = hashtab_first(stab->hashtab, &iter); curr; curr = hashtab_next(&iter)) {
        Symbol *sym = CONTAINER_OF(curr, Symbol, hash);        
        BackEndSymbol *bsym = bstab_lookup(bstab, curr->key);
        
        switch (sym->tag) {
            case ST_FUNCTION:   codegen_func_sym_to_backsym(&sym->func, bsym); break;
            case ST_STATIC_VAR: codegen_static_sym_to_backsym(sym, bsym); break;
            case ST_LOCAL_VAR:  codegen_local_sym_to_backsym(sym, bsym); break;
                break;
        }

    }
}

