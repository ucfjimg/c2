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
            case ASM_MOV: asm_fixop_mov(&newcode, node); break;

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
