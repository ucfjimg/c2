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

    if (tac->unary.op == UOP_LOGNOT) {

        codegen_push_instr(state, asm_cmp(aoper_imm(0), src, tac->loc));
        codegen_push_instr(state, asm_mov(aoper_imm(0), dst, tac->loc));
        codegen_push_instr(state, asm_setcc(aoper_clone(dst), ACC_E, tac->loc));

        return;
    }

    codegen_push_instr(state, asm_mov(src, dst, tac->loc));
    codegen_push_instr(state, asm_unary(tac->unary.op, aoper_clone(dst), tac->loc));
}

//
// Generate code for a relational operator.
//
void codegen_relational(CodegenState *state, TacNode *tac)
{
    AsmConditionCode cc = ACC_E;

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

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    codegen_push_instr(state, asm_cmp(right, left, tac->loc));
    codegen_push_instr(state, asm_mov(aoper_imm(0), dst, tac->loc));
    codegen_push_instr(state, asm_setcc(aoper_clone(dst), cc, tac->loc));
}

//
// Generate code for a binary operator.
//
void codegen_binary(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_BINARY);

    if (bop_is_relational(tac->binary.op)) {
        codegen_relational(state, tac);
        return;
    }

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
// Generate code for a jump instruction.
//
void codegen_jump(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_JUMP);

    codegen_push_instr(state, asm_jump(tac->jump.target, tac->loc));
}

//
// Generate code for a jump-on-zero instruction.
//
void codegen_jump_zero(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_JUMP_ZERO);

    AsmOperand *cond = codegen_expression(state, tac->jump_zero.condition);

    codegen_push_instr(state, asm_cmp(aoper_imm(0), cond, tac->loc));
    codegen_push_instr(state, asm_jumpcc(tac->jump_zero.target, ACC_E, tac->loc));
}

//
// Generate code for a jump-on-not-zero instruction.
//
void codegen_jump_not_zero(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_JUMP_NOT_ZERO);

    AsmOperand *cond = codegen_expression(state, tac->jump_zero.condition);

    codegen_push_instr(state, asm_cmp(aoper_imm(0), cond, tac->loc));
    codegen_push_instr(state, asm_jumpcc(tac->jump_zero.target, ACC_NE, tac->loc));
}

//
// Generate code for a copy instruction.
//
void codegen_copy(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_COPY);

    AsmOperand *src = codegen_expression(state, tac->copy.src);
    AsmOperand *dst = codegen_expression(state, tac->copy.dst);

    codegen_push_instr(state, asm_mov(src, dst, tac->loc));
}

//
// Generate code for a label.
//
void codegen_label(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_LABEL);

    codegen_push_instr(state, asm_label(tac->label.name, tac->loc));
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
// Generate code from the AST.
//
AsmNode *codegen(TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_PROGRAM);

    CodegenState state;
    list_clear(&state.code);

    for (ListNode *curr = tac->prog.decls.head; curr; curr = curr->next) {
        TacNode *decl = CONTAINER_OF(curr, TacNode, list);
        codegen_funcdef(&state, decl);
    }

    return asm_prog(state.code, tac->loc);
}
