#include "codegen.h"

#include "asm-ast.h"
#include "ice.h"
#include "list.h"

typedef struct {
    List code;
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

//
// Create a nested state.
//
static CodegenState nested_state(CodegenState *outer)
{
    CodegenState nested = *outer;
    return nested;
}

//
// Push an assembly instruction onto a code list.
//
static void codegen_push_instr(CodegenState *state, AsmNode *instr)
{
    list_push_back(&state->code, &instr->list);
}

static AsmOperand *codegen_expression(CodegenState *state, TacNode *tac);

//
// Generate code for a const int.
//
static AsmOperand *codegen_const_int(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_CONST_INT);
    return aoper_imm(tac->constint.val);
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
static void codegen_unary(CodegenState *state, TacNode *tac)
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
static void codegen_relational(CodegenState *state, TacNode *tac)
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
static void codegen_binary(CodegenState *state, TacNode *tac)
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
static void codegen_return(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_RETURN);

    AsmOperand *retval = codegen_expression(state, tac->ret.val);

    codegen_push_instr(state, asm_mov(retval, aoper_reg(REG_RAX), tac->loc));
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

    codegen_push_instr(state, asm_cmp(aoper_imm(0), cond, tac->loc));
    codegen_push_instr(state, asm_jumpcc(tac->jump_zero.target, ACC_E, tac->loc));
}

//
// Generate code for a jump-on-not-zero instruction.
//
static void codegen_jump_not_zero(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_JUMP_NOT_ZERO);

    AsmOperand *cond = codegen_expression(state, tac->jump_zero.condition);

    codegen_push_instr(state, asm_cmp(aoper_imm(0), cond, tac->loc));
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

    codegen_push_instr(state, asm_mov(src, dst, tac->loc));
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

    int arg_count = list_count(&call->args);
    int args_in_regs = arg_count < int_arg_reg_count ? arg_count : int_arg_reg_count;
    int args_on_stack = arg_count - args_in_regs;

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
    // Load register arguments.
    //
    ListNode *curr = call->args.head;
    for (int i = 0; i < args_in_regs; i++, curr = curr->next) {
        Register arg_reg = int_arg_regs[i];
        TacNode *tac_arg = CONTAINER_OF(curr, TacNode, list);
        AsmOperand *arg = codegen_expression(state, tac_arg);
        codegen_push_instr(state, asm_mov(arg, aoper_reg(arg_reg), tac->loc));
    }

    //
    // Push stack args.
    //
    list_reverse(&call->args);

    curr = call->args.head;
    for (int i = 0; i < args_on_stack; i++, curr = curr->next) {
        TacNode *tac_arg = CONTAINER_OF(curr, TacNode, list);
        AsmOperand *arg = codegen_expression(state, tac_arg);
        codegen_push_instr(state, asm_mov(arg, aoper_reg(REG_RAX), tac->loc));
        codegen_push_instr(state, asm_push(aoper_reg(REG_RAX), tac->loc));
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
    // Return value, if any, goes in RAX.
    //
    AsmOperand *result = codegen_expression(state, call->dst);
    codegen_push_instr(state, asm_mov(aoper_reg(REG_RAX), result, tac->loc));
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

        case TAC_PROGRAM:           break;
        case TAC_CONST_INT:         break;
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

    //
    // Move parameters into local pseudoregisters.
    //
    int param_count = list_count(&func->parms);
    int params_in_regs = param_count > int_arg_reg_count ? int_arg_reg_count : param_count;
    int params_on_stack = param_count - params_in_regs;

    //
    // Register based parameters.
    //
    ListNode *curr = func->parms.head;
    for (int i = 0; i < params_in_regs; i++, curr = curr->next) {
        TacFuncParam *param = CONTAINER_OF(curr, TacFuncParam, list);
        codegen_push_instr(
            &funcstate,
            asm_mov(
                aoper_reg(int_arg_regs[i]),
                aoper_pseudoreg(param->name),
                tac->loc
            )
        );
    }

    //
    // Stack based parameters.
    //
    int offset = 16;
    for (int i = 0; i < params_on_stack; i++, curr = curr->next) {
        TacFuncParam *param = CONTAINER_OF(curr, TacFuncParam, list);
        codegen_push_instr(
            &funcstate,
            asm_mov(
                aoper_stack(offset),
                aoper_pseudoreg(param->name),
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
