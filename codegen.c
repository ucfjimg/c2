#include "codegen.h"

#include "asm-ast.h"
#include "ice.h"
#include "list.h"

typedef struct {
    List code;
} CodegenState;

//
// Create a nested state.
//
CodegenState nested_state(CodegenState *outer)
{
    CodegenState nested = *outer;
    return nested;
}

//
// Push an assembly instruction onto a code list.
//
void codegen_push_instr(CodegenState *state, AsmNode *instr)
{
    list_push_back(&state->code, &instr->list);
}

static AsmOperand *codegen_expression(CodegenState *state, TacNode *tac);

//
// Generate code for a const int.
//
AsmOperand *codegen_const_int(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_CONST_INT);
    return aoper_imm(tac->constint.val);
}

//
// Generate code for a variable reference.
//
AsmOperand *codegen_var(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_VAR);
    return aoper_pseudoreg(tac->var.name);
}

//
// Generate code for an expression. Code to generate the value
// will be appended to state->code, and an unreferenced operand
// node containing the result location will be returned.
//
AsmOperand *codegen_expression(CodegenState *state, TacNode *tac)
{
    switch (tac->tag) {
        case TAC_CONST_INT: return codegen_const_int(state, tac);
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
// Generate code for a unary operator.
//
void codegen_unary(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_UNARY);

    AsmOperand *src = codegen_expression(state, tac->unary.src);
    AsmOperand *dst = codegen_expression(state, tac->unary.dst);

    codegen_push_instr(state, asm_mov(src, dst, tac->loc));
    codegen_push_instr(state, asm_unary(tac->unary.op, aoper_clone(dst), tac->loc));
}

//
// Generate code for a binary operator.
//
void codegen_binary(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_BINARY);

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    if (tac->binary.op == BOP_DIVIDE || tac->binary.op == BOP_MODULO) {
        codegen_push_instr(state, asm_mov(left, aoper_reg(REG_RAX), tac->loc));
        codegen_push_instr(state, asm_cdq(tac->loc));
        codegen_push_instr(state, asm_idiv(right, tac->loc));

        //
        // idiv returns quotient in AX, remainder in DX
        //
        Register dstreg = tac->binary.op == BOP_DIVIDE ? REG_RAX : REG_RDX;
        codegen_push_instr(state, asm_mov(aoper_reg(dstreg), dst, tac->loc));

        return;
    }

    codegen_push_instr(state, asm_mov(left, dst, tac->loc));
    dst = aoper_clone(dst);
    codegen_push_instr(state, asm_binary(tac->binary.op, right, dst, tac->loc));
}

//
// Generate code for a return instruction.
//
void codegen_return(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_RETURN);

    AsmOperand *retval = codegen_expression(state, tac->ret.val);

    codegen_push_instr(state, asm_mov(retval, aoper_reg(REG_RAX), tac->loc));
    codegen_push_instr(state, asm_ret(tac->loc));
}

//
// Generate code for a single instruction.
//
static void codegen_single(CodegenState *state, TacNode *tac)
{
    switch (tac->tag) {
        case TAC_UNARY:  codegen_unary(state, tac); break;
        case TAC_BINARY: codegen_binary(state, tac); break;
        case TAC_RETURN: codegen_return(state, tac); break;

    default:
        ICE_ASSERT(((void)"invalid TAC node in codegen_single", false));
    }
}

//
// Generate code for a function.
//
static void codegen_funcdef(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_FUNCDEF);

    CodegenState funcstate = nested_state(state);
    list_clear(&funcstate.code);

    for (ListNode *curr = tac->funcdef.body.head; curr; curr = curr->next) {
        TacNode *stmt = CONTAINER_OF(curr, TacNode, list);
        codegen_single(&funcstate, stmt);
    }

    codegen_push_instr(state, asm_func(tac->funcdef.name, funcstate.code, tac->loc));
}

//
// Generate code for an entire program.
//
static AsmNode *codegen_program(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_PROGRAM);
    codegen_funcdef(state, tac->prog.func);

    //
    // TODO this is hacky but will get fixed when the program is properly a list
    // of declarations.
    //
    AsmNode *prog = asm_prog(tac->loc);
    prog->prog.func = CONTAINER_OF(state->code.head, AsmNode, list);
    return prog;
}

//
// Generate code from the AST.
//
AsmNode *codegen(TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_PROGRAM);

    CodegenState state;
    list_clear(&state.code);

    return codegen_program(&state, tac);
}
