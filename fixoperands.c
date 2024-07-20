#include "fixoperands.h"

#include "asm-ast.h"
#include "codegen.h"
#include "ice.h"

//
// Fix a MOV instruction.
//
static void asm_fixop_mov(List *code, AsmNode *movnode)
{
    ICE_ASSERT(movnode->tag == ASM_MOV);
    AsmMov *mov = &movnode->mov;

    //
    // All pseudo-registers must have been replaced by the previous pass.
    //
    ICE_ASSERT(mov->src->tag != AOP_PSEUDOREG && mov->dst->tag != AOP_PSEUDOREG);

    //
    // MOV cannot have both operands as memory.
    //
    if (mov->src->tag == AOP_STACK && mov->dst->tag == AOP_STACK) {
        list_push_back(code, &asm_mov(aoper_clone(mov->src), aoper_reg(REG_R10), movnode->loc)->list);
        list_push_back(code, &asm_mov(aoper_reg(REG_R10), aoper_clone(mov->dst), movnode->loc)->list);

        asm_free(movnode);
        return;
    }

    list_push_back(code, &movnode->list);
}

//
// Fix an IDIV instruction.
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
        list_push_back(code, &asm_mov(aoper_clone(idiv->arg), aoper_reg(REG_R10), idivnode->loc)->list);
        list_push_back(code, &asm_idiv(aoper_reg(REG_R10), idivnode->loc)->list);

        asm_free(idivnode);

        return;
    }
    

    list_push_back(code, &idivnode->list);
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

    //
    // ADD, SUB, XOR, AND, OR cannot have both instructions as memory operands.
    //
    if ((binop->op == BOP_ADD || 
         binop->op == BOP_SUBTRACT || 
         binop->op == BOP_BITAND || 
         binop->op == BOP_BITOR ||
         binop->op == BOP_BITXOR) &&
        binop->src->tag == AOP_STACK &&
        binop->dst->tag == AOP_STACK) {
              
        list_push_back(code, &asm_mov(aoper_clone(binop->src), aoper_reg(REG_R10), binopnode->loc)->list);
        list_push_back(code, &asm_binary(binop->op, aoper_reg(REG_R10), aoper_clone(binop->dst), binopnode->loc)->list);
  
        asm_free(binopnode);

        return;
    }

    //
    // IMUL can't have a memory operand as its destination
    //
    if (binop->op == BOP_MULTIPLY && binop->dst->tag == AOP_STACK) {

        list_push_back(code, &asm_mov(aoper_clone(binop->dst), aoper_reg(REG_R11), binopnode->loc)->list);
        list_push_back(code, &asm_binary(
            BOP_MULTIPLY, aoper_clone(binop->src), aoper_reg(REG_R11), binopnode->loc)->list);
        list_push_back(code, &asm_mov(aoper_reg(REG_R11), aoper_clone(binop->dst), binopnode->loc)->list);

        asm_free(binopnode);

        return;
    }

    //
    // SHR/SAR/SHL can't have a memory shift count, and the shift count must be in CL if
    // a register is needed.
    //
    if ((binop->op == BOP_LSHIFT || binop->op == BOP_RSHIFT) && binop->src->tag == AOP_STACK) {
        list_push_back(code, &asm_mov(aoper_clone(binop->src), aoper_reg(REG_RCX), binopnode->loc)->list);
        list_push_back(code, &asm_binary(binop->op, aoper_reg(REG_RCX), aoper_clone(binop->dst), binopnode->loc)->list);

        asm_free(binopnode);

        return;
    }

    list_push_back(code, &binopnode->list);
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
        AsmNode *reserve = asm_stack_reserve(func->func.locals_size, func->loc);
        list_push_front(&newcode, &reserve->list);
    }

    for (ListNode *curr = func->func.body.head; curr; ) {
        ListNode *next = curr->next;        
        AsmNode *node = CONTAINER_OF(curr, AsmNode, list);

        switch (node->tag)
        {
            case ASM_MOV:       asm_fixop_mov(&newcode, node); break;
            case ASM_IDIV:      asm_fixop_idiv(&newcode, node); break;
            case ASM_BINARY:    asm_fixop_binary(&newcode, node); break;

            default:
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

    asm_fixop_func(prog->prog.func);
}
