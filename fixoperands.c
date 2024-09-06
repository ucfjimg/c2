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

    Register reg = mov->type->tag == AT_DOUBLE ? REG_XMM15 : REG_R10;

    
    //
    // All pseudo-registers must have been replaced by the previous pass.
    //
    ICE_ASSERT(mov->src->tag != AOP_PSEUDOREG && mov->dst->tag != AOP_PSEUDOREG);

    if (needs_reg) {
        AsmType *at = asmtype_clone(mov->type);
        list_push_back(code, &asm_mov(aoper_clone(mov->src), aoper_reg(reg), at, movnode->loc)->list);

        at = asmtype_clone(mov->type);
        list_push_back(code, &asm_mov(aoper_reg(reg), aoper_clone(mov->dst), at, movnode->loc)->list);

        asm_free(movnode);
        return;
    }

    list_push_back(code, &movnode->list);
}

//
// Fix a MOVSX instruction.
//
// - source operand cannot be an immediate.
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
        list_push_back(code, &asm_mov(movsx->src, aoper_reg(REG_R10), asmtype_clone(movsx->src_type), movnode->loc)->list);
        movsx->src = aoper_reg(REG_R10);
    }

    if (aoper_is_mem(movsx->dst)) {
        dst = movsx->dst;
        movsx->dst = aoper_reg(REG_R11);
    }

    list_push_back(code, &movnode->list);

    if (dst) {
        list_push_back(code, &asm_mov(aoper_reg(REG_R11), dst, asmtype_clone(movsx->dst_type), movnode->loc)->list);
    }
}

//
// Fix a MOVZX instruction.
//
// This just uses the inherent behavior of the `mov` instruction to zero pad the
// top bits of 64-bit register when loaded from a 32-bit quantity. 
//
static void asm_fixop_movzx(List *code, AsmNode *movnode)
{
    ICE_ASSERT(movnode->tag == ASM_MOVZX);
    AsmMovzx *movzx = &movnode->movzx;
    
    //
    // All pseudo-registers must have been replaced by the previous pass.
    //
    ICE_ASSERT(movzx->src->tag != AOP_PSEUDOREG && movzx->dst->tag != AOP_PSEUDOREG);

    if (movzx->src_type->tag == AT_BYTE) {
        if (movzx->src->tag == AOP_IMM) {
            list_push_back(code, 
                &asm_mov(aoper_imm(movzx->src->imm & 0xff), aoper_reg(REG_R10), asmtype_byte(), movnode->loc)->list);
            movzx->src = aoper_reg(REG_R10);
        }

        AsmOperand *dst = NULL;
        if (movzx->dst->tag != AOP_REG) {
            dst = movzx->dst;
            movzx->dst = aoper_reg(REG_R11);
        }

        list_push_back(code, &movnode->list);

        if (dst) {
            list_push_back(code, 
                &asm_mov(aoper_reg(REG_R11), dst, asmtype_clone(movzx->dst_type), movnode->loc)->list);
        }

        return;
    }

    if (movzx->dst->tag == AOP_REG) {
        list_push_back(code, &asm_mov(aoper_clone(movzx->src), aoper_clone(movzx->dst), asmtype_long(), movnode->loc)->list);
    } else {
        list_push_back(code, &asm_mov(aoper_clone(movzx->src), aoper_reg(REG_R10), asmtype_long(), movnode->loc)->list);
        list_push_back(code, &asm_mov(aoper_reg(REG_R10), aoper_clone(movzx->dst), asmtype_quad(), movnode->loc)->list);

    }

    asm_free(movnode);
}

//
// Fix an LEA instruction.
//
// - destination operand must be a register.
//
static void asm_fixop_lea(List *code, AsmNode *leanode)
{
    ICE_ASSERT(leanode->tag == ASM_LEA);
    AsmLea *lea = &leanode->lea;
    
    //
    // All pseudo-registers must have been replaced by the previous pass.
    //
    ICE_ASSERT(lea->src->tag != AOP_PSEUDOREG && lea->dst->tag != AOP_PSEUDOREG);

    if (lea->dst->tag != AOP_REG) {
        AsmOperand *dst = lea->dst;
        lea->dst = aoper_reg(REG_R10);
        list_push_back(code, &leanode->list);        
        list_push_back(code, &asm_mov(aoper_reg(REG_R10), dst, asmtype_quad(), leanode->loc)->list);
        return;
    }

    list_push_back(code, &leanode->list);
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
// Fix a DIV instruction.
//
// - source operand cannot be an immediate. 
// 
static void asm_fixop_div(List *code, AsmNode *divnode)
{
    ICE_ASSERT(divnode->tag == ASM_DIV);
    AsmDiv *div = &divnode->div;

    //
    // All pseudo-registers must have been replaced by the previous pass.
    //
    ICE_ASSERT(div->arg->tag != AOP_PSEUDOREG);

    //
    // IDIV cannot take a constant argument.
    //
    if (div->arg->tag == AOP_IMM) {
        AsmType *at = asmtype_clone(div->type);
        list_push_back(code, &asm_mov(aoper_clone(div->arg), aoper_reg(REG_R10), at, divnode->loc)->list);
        at = asmtype_clone(at);
        list_push_back(code, &asm_idiv(aoper_reg(REG_R10), at, divnode->loc)->list);

        asm_free(divnode);

        return;
    }
    

    list_push_back(code, &divnode->list);
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
// Fix floating point arith operators.
//
// - destination must be a register.
//
static void asm_fixop_float_arith(List *code, AsmNode *binopnode)
{
    ICE_ASSERT(binopnode->tag == ASM_BINARY);
    AsmBinary *binop = &binopnode->binary;
    FileLine loc = binopnode->loc;

    if (binop->dst->tag != AOP_REG) {
        AsmOperand *dst = binop->dst;
        
        binop->dst = aoper_reg(REG_XMM15);

        list_push_back(code, &asm_mov(dst, aoper_reg(REG_XMM15), asmtype_double(), loc)->list);
        list_push_back(code, &binopnode->list);
        list_push_back(code, &asm_mov(aoper_reg(REG_XMM15), aoper_clone(dst), asmtype_double(), loc)->list);
    } else {
        list_push_back(code, &binopnode->list);
    }
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

    if (binop->type->tag == AT_DOUBLE) {
        switch (binop->op) {
            case BOP_ADD:
            case BOP_SUBTRACT: 
            case BOP_MULTIPLY:
            case BOP_DIVIDE:
            case BOP_DIVDBL:
            case BOP_BITAND: 
            case BOP_BITOR:
            case BOP_BITXOR:
                asm_fixop_float_arith(code, binopnode);
                break;

            default:
                list_push_back(code, &binopnode->list);
                break;
        }
    } else {
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

            default:
                //
                // Most operations need no fixup.
                //
                list_push_back(code, &binopnode->list);
                break;
        }
    }
}

// 
// Fix up a floating point (comisd) compare instruction.
//
// - Destination must be a register.
//
static void asm_fixop_comisd(List *code, AsmNode *cmpnode)
{
    ICE_ASSERT(cmpnode->tag == ASM_CMP);
    AsmCmp *cmp = &cmpnode->cmp;
    FileLine loc = cmpnode->loc;

    if (cmp->right->tag != AOP_REG) {
        list_push_back(code, &asm_mov(cmp->right, aoper_reg(REG_XMM15), asmtype_double(), loc)->list);
        cmp->right = aoper_reg(REG_XMM15);
    }

    list_push_back(code, &cmpnode->list);
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

    if (at->tag == AT_DOUBLE) {
        asm_fixop_comisd(code, cmpnode);
        return;
    }

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
// Fix up a cvttsd2si instruction.
//
// - Destination must be a register.
//
static void asm_fixop_cvttsd2si(List *code, AsmNode *cvtnode)
{
    ICE_ASSERT(cvtnode->tag == ASM_CVTTSD2SI);
    AsmCvttsd2si *cvt = &cvtnode->cvttsd2si;

    if (cvt->dst->tag != AOP_REG) {
        AsmOperand *dst = cvt->dst;
        cvt->dst = aoper_reg(REG_R11);
        list_push_back(code, &cvtnode->list);
        list_push_back(code, &asm_mov(aoper_reg(REG_R11), dst, asmtype_clone(cvt->type), cvtnode->loc)->list);
        return;
    }
    list_push_back(code, &cvtnode->list);
}

//
// Fix up a cvtsi2sd instruction.
//
// - Source cannot be an immediate.
// - Destination must be a register.
//
static void asm_fixop_cvtsi2sd(List *code, AsmNode *cvtnode)
{
    ICE_ASSERT(cvtnode->tag == ASM_CVTSI2SD);
    AsmCvtsi2sd *cvt = &cvtnode->cvtsi2sd;
    AsmOperand *dst = NULL;

    if (cvt->src->tag == AOP_IMM) {
        list_push_back(code, &asm_mov(cvt->src, aoper_reg(REG_R10), asmtype_clone(cvt->type), cvtnode->loc)->list);
        cvt->src = aoper_reg(REG_R10);
    }

    if (aoper_is_mem(cvt->dst)) {
        dst = cvt->dst;
        cvt->dst = aoper_reg(REG_XMM15);
    }

    list_push_back(code, &cvtnode->list);

    if (dst) {
        list_push_back(code, &asm_mov(aoper_reg(REG_XMM15), dst, asmtype_double(), cvtnode->loc)->list);
    }
}

//
// Fix up a push instruction.
//
static void asm_fixop_push(List *code, AsmNode *pushnode)
{
    ICE_ASSERT(pushnode->tag == ASM_PUSH);
    AsmPush *push = &pushnode->push;

    if (push->value->tag == AOP_REG && is_xmm(push->value->reg)) {
        list_push_back(code, &asm_binary(BOP_SUBTRACT, aoper_imm(8), aoper_reg(REG_RSP), asmtype_quad(), pushnode->loc)->list);
        list_push_back(code, &asm_mov(aoper_reg(push->value->reg), aoper_memory(REG_RSP, 0), asmtype_double(), pushnode->loc)->list);
        return;
    }

    list_push_back(code, &pushnode->list);
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
            case ASM_MOVZX:     asm_fixop_movzx(&newcode, node); break;
            case ASM_LEA:       asm_fixop_lea(&newcode, node); break;
            case ASM_IDIV:      asm_fixop_idiv(&newcode, node); break;
            case ASM_DIV:       asm_fixop_div(&newcode, node); break;
            case ASM_BINARY:    asm_fixop_binary(&newcode, node); break;
            case ASM_CMP:       asm_fixop_cmp(&newcode, node); break;

            case ASM_CVTTSD2SI: asm_fixop_cvttsd2si(&newcode, node); break;
            case ASM_CVTSI2SD:  asm_fixop_cvtsi2sd(&newcode, node); break;

            case ASM_PUSH:      asm_fixop_push(&newcode, node); break;

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
            case ASM_STATIC_CONST:
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
