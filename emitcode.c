#include "emitcode.h"

#include "fileline.h"
#include "ice.h"
#include "safemem.h"

static void emitcode_recurse(FILE *out, AsmNode *node, FileLine *loc);

//
// Emit a register value.
//
static void emit_reg(FILE *out, Register reg)
{
    switch (reg) {
        case REG_RAX: fprintf(out, "%%eax"); break;
        case REG_RDX: fprintf(out, "%%edx"); break;
        case REG_R10: fprintf(out, "%%r10d"); break;
        case REG_R11: fprintf(out, "%%r11d"); break;
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
// Emit a stack frame reference.
//
static void emit_stack(FILE *out, int offset)
{
    fprintf(out, "%d(%%rbp)", offset);
}

//
// Emit an assembly operand.
//
static void emit_asmoper(FILE *out, AsmOperand *oper)
{
    switch (oper->tag) {
        case AOP_IMM:   emit_imm(out, oper->imm); break;
        case AOP_REG:   emit_reg(out, oper->reg); break;
        case AOP_STACK: emit_stack(out, oper->stack_offset); break;
        
        case AOP_PSEUDOREG:
            ICE_ASSERT(((void)"pseuedoreg operand found at code emission time.", false));
    }
}

//
// Emit a return instruction.
//
static void emit_ret(FILE *out)
{
    fprintf(out, "        movq %%rbp, %%rsp\n");
    fprintf(out, "        popq %%rbp\n");
    fprintf(out, "        ret\n");
}

//
// Emit a CDQ instruction.
//
static void emit_cdq(FILE *out)
{
    fprintf(out, "        cdq\n");
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
// Emit a unary instruction.
//
static void emit_unary(FILE *out, AsmUnary *unary)
{
    char *opcode = "???";

    switch (unary->op) {
        case UOP_PLUS:          return;
        case UOP_MINUS:         opcode = "neg"; break;
        case UOP_COMPLEMENT:    opcode = "not"; break;

        default:
            ICE_ASSERT(((void)"invalid unary opcode in emit_unary", false));
    }

    fprintf(out, "        %sl ", opcode);
    emit_asmoper(out, unary->arg);
    fprintf(out, "\n");
}

//
// Emit a binary instruction.
//
static void emit_binary(FILE *out, AsmBinary *binary)
{
    char *opcode = "???";

    switch (binary->op) {
        case BOP_ADD:           opcode = "add"; break;
        case BOP_SUBTRACT:      opcode = "sub"; break;
        case BOP_MULTIPLY:      opcode = "imul"; break;

        //
        // NOTE idiv is handled as a special case.
        //
        default:
            ICE_ASSERT(((void)"invalid binary opcode in emit_binary", false));
    }

    fprintf(out, "        %sl ", opcode);
    emit_asmoper(out, binary->src);
    fprintf(out, ", ");
    emit_asmoper(out, binary->dst);
    fprintf(out, "\n");
}

//
// Emit an IDIV instruction.
//
static void emit_idiv(FILE *out, AsmIdiv *idiv)
{
    fprintf(out, "        idivl ");
    emit_asmoper(out, idiv->arg);
    fprintf(out, "\n");
}

//
// Emit code to reserve locals on the stack.
//
static void emit_stack_reserve(FILE *out, AsmStackReserve *reserve)
{
    fprintf(out, "        subq $%d, %%rsp\n", reserve->bytes);
}

//
// Emit a function.
//
static void emit_function(FILE *out, AsmFunction *func, FileLine *loc)
{
#ifdef __APPLE__
    fprintf(out, "        .globl _%s\n", func->name);
    fprintf(out, "_%s:\n", func->name);
#else   
    fprintf(out, "        .globl %s\n", func->name);
    fprintf(out, "%s:\n", func->name);
#endif
    fprintf(out, "        pushq %%rbp\n");
    fprintf(out, "        movq %%rsp, %%rbp\n");

    for (ListNode *curr = func->body.head; curr; curr = curr->next) {
        AsmNode *node = CONTAINER_OF(curr, AsmNode, list);
        emitcode_recurse(out, node, loc);
    }    
}

//
// emit a top-level program.
// 
static void emit_program(FILE *out, AsmProgram *prog, FileLine *loc)
{
    emitcode_recurse(out, prog->func, loc);

#ifndef __APPLE__
    fprintf(out, "        .section .note.GNU-stack,\"\",@progbits\n");
#endif
}

//
// Write actual assembly code out.
//
static void emitcode_recurse(FILE *out, AsmNode *node, FileLine *loc)
{
    if (node->loc.fname != loc->fname || node->loc.line != loc->line) {
        *loc = node->loc;
        char *sloc = fileline_describe(loc);
        fprintf(out, "# %s\n", sloc);
        safe_free(sloc);
    }

    switch (node->tag) {
        case ASM_PROG:          emit_program(out, &node->prog, loc); break;
        case ASM_FUNC:          emit_function(out, &node->func, loc); break;
        case ASM_STACK_RESERVE: emit_stack_reserve(out, &node->stack_reserve); break;
        case ASM_MOV:           emit_mov(out, &node->mov); break;
        case ASM_UNARY:         emit_unary(out, &node->unary); break;
        case ASM_BINARY:        emit_binary(out, &node->binary); break;
        case ASM_RET:           emit_ret(out); break;
        case ASM_CDQ:           emit_cdq(out); break;
        case ASM_IDIV:          emit_idiv(out, &node->idiv); break;
    }
}

//
// Top level entry to emit code.
//
void emitcode(FILE *out, AsmNode *node)
{
    FileLine loc = { NULL, 0 };

    emitcode_recurse(out, node, &loc);
}
