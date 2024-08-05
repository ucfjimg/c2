#include "fixoperands.h"

#include "asm-ast.h"
#include "bitmath.h"
#include "codegen.h"
#include "ice.h"

#include <stdint.h>

//
// This pass fixs the assembly AST to conform with x64 instruction encoding
// constraints.
//

//
// Returns true if the given immediate value, cast as a signed,
// fits in the range of 32-bit signed int.
//
static bool imm_fits_in_int(unsigned long imm)
{
    signed long simm = (signed long)imm;

    return simm >= INT32_MIN && simm <= INT32_MAX;
}

//
// Fix a MOV instruction.
//
// - cannot have both operands in memory.
// - if the source is an immediate greater than 32 bits, the destination must be a register.
//
static void asm_fixop_mov(List *code, AsmNode *movnode)
{
    ICE_ASSERT(movnode->tag == ASM_MOV);
    AsmMov *mov = &movnode->mov;

    bool needs_reg = 
        (aoper_is_mem(mov->src) && aoper_is_mem(mov->dst)) ||
        (mov->src->tag == AOP_IMM && !imm_fits_in_int(mov->src->imm));

    //
    // All pseudo-registers must have been replaced by the previous pass.
    //
    ICE_ASSERT(mov->src->tag != AOP_PSEUDOREG && mov->dst->tag != AOP_PSEUDOREG);

    if (needs_reg) {
        AsmType *at = asmtype_clone(mov->type);
        list_push_back(code, &asm_mov(aoper_clone(mov->src), aoper_reg(REG_R10), at, movnode->loc)->list);

        at = asmtype_clone(mov->type);
        list_push_back(code, &asm_mov(aoper_reg(REG_R10), aoper_clone(mov->dst), at, movnode->loc)->list);

        asm_free(movnode);
        return;
    }

    list_push_back(code, &movnode->list);
}

//
// Fix a MOVSX instruction.
//
// - source operand cannot be an immdiate.
// - destination operand cannot be in memory.
//
static void asm_fixop_movsx(List *code, AsmNode *movnode)
{
    ICE_ASSERT(movnode->tag == ASM_MOVSX);
    AsmMovsx *movsx = &movnode->movsx;
    AsmOperand *dst = NULL;
    
    //
    // All pseudo-registers must have been replaced by the previous pass.
    //
    ICE_ASSERT(movsx->src->tag != AOP_PSEUDOREG && movsx->dst->tag != AOP_PSEUDOREG);

    if (movsx->src->tag == AOP_IMM) {
        list_push_back(code, &asm_mov(movsx->src, aoper_reg(REG_R10), asmtype_long(), movnode->loc)->list);
        movsx->src = aoper_reg(REG_R10);
    }

    if (aoper_is_mem(movsx->dst)) {
        dst = movsx->dst;
        movsx->dst = aoper_reg(REG_R11);
    }

    list_push_back(code, &movnode->list);

    list_push_back(code, &asm_mov(aoper_reg(REG_R11), dst, asmtype_quad(), movnode->loc)->list);
}

//
// Fix an IDIV instruction.
//
// - source operand cannot be an immediate. 
// 
static void asm_fixop_idiv(List *code, AsmNode *idivnode)
{
    ICE_ASSERT(idivnode->tag == ASM_IDIV);
    AsmIdiv *idiv = &idivnode->idiv;

    //
    // All pseudo-registers must have been replaced by the previous pass.
    //
    ICE_ASSERT(idiv->arg->tag != AOP_PSEUDOREG);

    //
    // IDIV cannot take a constant argument.
    //
    if (idiv->arg->tag == AOP_IMM) {
        AsmType *at = asmtype_clone(idiv->type);
        list_push_back(code, &asm_mov(aoper_clone(idiv->arg), aoper_reg(REG_R10), at, idivnode->loc)->list);
        at = asmtype_clone(at);
        list_push_back(code, &asm_idiv(aoper_reg(REG_R10), at, idivnode->loc)->list);

        asm_free(idivnode);

        return;
    }
    

    list_push_back(code, &idivnode->list);
}

//
// Fix an IMUL instruction.
//
// - if source operand is immediate, it must fit in 32 bits.
// - destinatation operand cannot be memory.
//
static void asm_fixop_imul(List *code, AsmNode *binopnode)
{
    ICE_ASSERT(binopnode->tag == ASM_BINARY);
    AsmBinary *binop = &binopnode->binary;
    ICE_ASSERT(binop->op == BOP_MULTIPLY);

    //
    // IMUL can't have a memory operand as its destination, nor an immediate value
    // that won't fit in s32 as a source.
    //
    AsmType *at = asmtype_clone(binop->type);
    
    if (binop->src->tag == AOP_IMM && !imm_fits_in_int(binop->src->imm)) {
        list_push_back(code, &asm_mov(binop->src, aoper_reg(REG_R10), asmtype_clone(at), binopnode->loc)->list);
        binop->src = aoper_reg(REG_R10);
    }
    
    if (aoper_is_mem(binop->dst)) {
        list_push_back(code, &asm_mov(aoper_clone(binop->dst), aoper_reg(REG_R11), at, binopnode->loc)->list);
        list_push_back(code, &asm_binary(
            BOP_MULTIPLY, aoper_clone(binop->src), aoper_reg(REG_R11), asmtype_clone(at), binopnode->loc)->list);
        list_push_back(code, &asm_mov(aoper_reg(REG_R11), aoper_clone(binop->dst), asmtype_clone(at), binopnode->loc)->list);

        asm_free(binopnode);
    } else {
        list_push_back(code, &binopnode->list);
    }
}

//
// Fix an arithmetic binary operator. All of these operators have similar
// encoding constraints.
//
// - cannot have two memory operands
// - an immediate operands must fit in 32 bits.
//
static void ast_fixup_binary_arith(List *code, AsmNode *binopnode)
{
    ICE_ASSERT(binopnode->tag == ASM_BINARY);
    AsmBinary *binop = &binopnode->binary;
    AsmType *at = binop->type;
    AsmOperand *dst = binop->dst;

    bool both_mem = aoper_is_mem(binop->src) && aoper_is_mem(binop->dst);

    if (both_mem || (binop->src->tag == AOP_IMM && !imm_fits_in_int(binop->src->imm))) {
        list_push_back(code, &asm_mov(binop->src, aoper_reg(REG_R10), asmtype_clone(at), binopnode->loc)->list);
        binop->src = aoper_reg(REG_R10);
    }

    if (both_mem) {
        list_push_back(code, &asm_mov(dst, aoper_reg(REG_R11), asmtype_clone(at), binopnode->loc)->list);
        binop->dst = aoper_reg(REG_R11);
    }

    list_push_back(code, &binopnode->list);

    if (both_mem) {
        list_push_back(code, &asm_mov(aoper_reg(REG_R11), aoper_clone(dst), asmtype_clone(at), binopnode->loc)->list);
        binop->dst = aoper_reg(REG_R11);
    }
}

//
// Fix a shift operator.
//
// - shift count must be in CL register.
//
static void asm_fixop_shift(List *code, AsmNode *binopnode)
{
    ICE_ASSERT(binopnode->tag == ASM_BINARY);
    AsmBinary *binop = &binopnode->binary;

    AsmType *at = asmtype_clone(binop->type);
    list_push_back(code, &asm_mov(aoper_clone(binop->src), aoper_reg(REG_RCX), at, binopnode->loc)->list);

    binop->src = aoper_reg(REG_RCX);
    list_push_back(code, &binopnode->list);
}

//
// Fix operators for a binary operator instruction.
//
static void asm_fixop_binary(List *code, AsmNode *binopnode)
{
    ICE_ASSERT(binopnode->tag == ASM_BINARY);
    AsmBinary *binop = &binopnode->binary;

    //
    // All pseudo-registers must have been replaced by the previous pass.
    //
    ICE_ASSERT(binop->src->tag != AOP_PSEUDOREG);
    ICE_ASSERT(binop->dst->tag != AOP_PSEUDOREG);

    switch (binop->op) {
        case BOP_ADD:
        case BOP_SUBTRACT: 
        case BOP_BITAND: 
        case BOP_BITOR:
        case BOP_BITXOR:
            ast_fixup_binary_arith(code, binopnode);
            break;

        case BOP_MULTIPLY:
            asm_fixop_imul(code, binopnode);
            break;

        case BOP_LSHIFT:
        case BOP_RSHIFT:
            asm_fixop_shift(code, binopnode);
            break;

        default:
            //
            // Most operations need no fixup.
            //
            list_push_back(code, &binopnode->list);
    }
}

//
// Fix up a compare intruction.
//
// - Left and right may not both be memory operands.
// - If left is immediate, it must fit in 32 bits.
// - Right may not be immediate.
//
static void asm_fixop_cmp(List *code, AsmNode *cmpnode)
{
    ICE_ASSERT(cmpnode->tag == ASM_CMP);
    AsmCmp *cmp = &cmpnode->cmp;
    AsmType *at = cmp->type;
    FileLine loc = cmpnode->loc;

    bool both_mem = aoper_is_mem(cmp->left) && aoper_is_mem(cmp->right);
    bool left_imm = cmp->left->tag == AOP_IMM && !imm_fits_in_int(cmp->left->imm);
    bool right_imm = cmp->right->tag == AOP_IMM;

    if (both_mem || left_imm) {
        list_push_back(code, &asm_mov(aoper_clone(cmp->left), aoper_reg(REG_R10), asmtype_clone(at), loc)->list);
        cmp->left = aoper_reg(REG_R10);
    }

    if (both_mem || right_imm) {
        list_push_back(code, &asm_mov(aoper_clone(cmp->right), aoper_reg(REG_R11), asmtype_clone(at), loc)->list);
        cmp->right = aoper_reg(REG_R11);
    }
    
    list_push_back(code, &cmpnode->list);
}

//
// Fix instructions in a function.
//
static void asm_fixop_func(AsmNode *func)
{
    ICE_ASSERT(func->tag == ASM_FUNC);

    //
    // Some instructions in the function will be replaced by 
    // one or more new instructions.
    //
    List newcode;
    list_clear(&newcode);

    //
    // If the function requires locals, reserve space for them.
    //
    if (func->func.locals_size) {
        int reserve_bytes = align_up(func->func.locals_size, 16);
        AsmNode *reserve = asm_stack_reserve(reserve_bytes, func->loc);
        list_push_front(&newcode, &reserve->list);
    }

    for (ListNode *curr = func->func.body.head; curr; ) {
        ListNode *next = curr->next;        
        AsmNode *node = CONTAINER_OF(curr, AsmNode, list);

        switch (node->tag)
        {
            case ASM_MOV:       asm_fixop_mov(&newcode, node); break;
            case ASM_MOVSX:     asm_fixop_movsx(&newcode, node); break;
            case ASM_IDIV:      asm_fixop_idiv(&newcode, node); break;
            case ASM_BINARY:    asm_fixop_binary(&newcode, node); break;
            case ASM_CMP:       asm_fixop_cmp(&newcode, node); break;

            case ASM_PUSH:
            case ASM_CALL:
            case ASM_PROG:
            case ASM_FUNC:
            case ASM_UNARY:
            case ASM_CDQ:
            case ASM_JUMP:
            case ASM_JUMPCC:
            case ASM_LABEL:
            case ASM_SETCC:
            case ASM_RET:
            case ASM_STACK_RESERVE:
            case ASM_STACK_FREE:
            case ASM_STATIC_VAR:
                //
                // instructions which need no modification
                //
                list_push_back(&newcode, &node->list);
                break;
        }

        curr = next;
    }

    //
    // At this point, all instruction nodes in the original body have
    // either been free'd or put into the new code list.
    //
    func->func.body = newcode;
}

//
// Code generation pass to fix instructions which violate target
// operand constraints.
//
void asm_fix_operands(AsmNode *prog)
{
    ICE_ASSERT(prog->tag == ASM_PROG);

    for (ListNode *curr = prog->prog.funcs.head; curr; curr = curr->next) {
        AsmNode *func = CONTAINER_OF(curr, AsmNode, list);
        if (func->tag == ASM_FUNC) {
            asm_fixop_func(func);
        }
    }
}
