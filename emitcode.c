#include "emitcode.h"

//
// Emit a register value.
//
static void emit_reg(FILE *out, Register reg)
{
    switch (reg) {
        case REG_RAX: fprintf(out, "%%eax"); break;
    }
}

//
// Emit an immediate value.
//
static void emit_imm(FILE *out, unsigned long val)
{
    fprintf(out, "$%lu", val);
}

//
// Emit an assembly operand.
//
static void emit_asmoper(FILE *out, AsmOperand *oper)
{
    switch (oper->tag) {
        case AOP_IMM: emit_imm(out, oper->imm); break;
        case AOP_REG: emit_reg(out, oper->reg); break;
    }
}

//
// Emit a return instruction.
//
static void emit_ret(FILE *out)
{
    fprintf(out, "        ret\n");
}

//
// Emit a mov instruction.
//
static void emit_mov(FILE *out, AsmMov *mov)
{
    fprintf(out, "        movl ");
    emit_asmoper(out, mov->src);
    fprintf(out, ", ");
    emit_asmoper(out, mov->dst);
    fprintf(out, "\n");
}

//
// Emit a function.
//
static void emit_function(FILE *out, AsmFunction *func)
{
    fprintf(out, "        .globl %s\n", func->name);
    fprintf(out, "%s:\n", func->name);

    for (ListNode *curr = func->body.head; curr; curr = curr->next) {
        AsmNode *node = CONTAINER_OF(curr, AsmNode, list);
        emitcode(out, node);
    }
}

//
// emit a top-level program.
// 
static void emit_program(FILE *out, AsmProgram *prog)
{
    emitcode(out, prog->func);

    fprintf(out, "        .section .note.GNU-stack,\"\",@progbits\n");
}

//
// Write actual assembly code out.
//
void emitcode(FILE *out, AsmNode *node)
{
    switch (node->tag) {
        case ASM_PROG: emit_program(out, &node->prog); break;
        case ASM_FUNC: emit_function(out, &node->func); break;
        case ASM_MOV:  emit_mov(out, &node->mov); break;
        case ASM_RET:  emit_ret(out); break;
    }
}
