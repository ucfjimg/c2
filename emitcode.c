#include "emitcode.h"

#include "fileline.h"
#include "ice.h"
#include "safemem.h"

static void emitcode_recurse(FILE *out, AsmNode *node, FileLine *loc);

typedef enum {
    OS_BYTE = 1,
    OS_DWORD = 4,
    OS_QWORD = 8,
} OperandSize;

//
// Emit a register value.
//
static void emit_reg(FILE *out, Register reg, OperandSize os)
{
    if (os == OS_QWORD) {
        switch (reg) {
            case REG_RAX: fprintf(out, "%%rax"); break;
            case REG_RCX: fprintf(out, "%%rcx"); break;
            case REG_RDX: fprintf(out, "%%rdx"); break;
            case REG_RDI: fprintf(out, "%%rdi"); break;
            case REG_RSI: fprintf(out, "%%rsi"); break;
            case REG_R8:  fprintf(out, "%%r8"); break;
            case REG_R9:  fprintf(out, "%%r9"); break;
            case REG_R10: fprintf(out, "%%r10"); break;
            case REG_R11: fprintf(out, "%%r11"); break;
        }
    } else if (os == OS_DWORD) {
        switch (reg) {
            case REG_RAX: fprintf(out, "%%eax"); break;
            case REG_RCX: fprintf(out, "%%ecx"); break;
            case REG_RDX: fprintf(out, "%%edx"); break;
            case REG_RDI: fprintf(out, "%%edi"); break;
            case REG_RSI: fprintf(out, "%%esi"); break;
            case REG_R8:  fprintf(out, "%%r8d"); break;
            case REG_R9:  fprintf(out, "%%r9d"); break;
            case REG_R10: fprintf(out, "%%r10d"); break;
            case REG_R11: fprintf(out, "%%r11d"); break;
        }
    } else if (os == OS_BYTE) {
        switch (reg) {
            case REG_RAX: fprintf(out, "%%al"); break;
            case REG_RCX: fprintf(out, "%%cl"); break;
            case REG_RDX: fprintf(out, "%%dl"); break;
            case REG_RDI: fprintf(out, "%%dil"); break;
            case REG_RSI: fprintf(out, "%%sil"); break;
            case REG_R8:  fprintf(out, "%%r8b"); break;
            case REG_R9:  fprintf(out, "%%r9b"); break;
            case REG_R10: fprintf(out, "%%r10b"); break;
            case REG_R11: fprintf(out, "%%r11b"); break;
        }
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
static void emit_asmoper(FILE *out, AsmOperand *oper, OperandSize os)
{
    switch (oper->tag) {
        case AOP_IMM:   emit_imm(out, oper->imm); break;
        case AOP_REG:   emit_reg(out, oper->reg, os); break;
        case AOP_STACK: emit_stack(out, oper->stack_offset); break;
        
        case AOP_PSEUDOREG:
            ICE_ASSERT(((void)"pseuedoreg operand found at code emission time.", false));
    }
}

//
// Emit a label.
// 
static void emit_label(FILE *out, const char *label)
{
#ifndef __APPLE__
    fprintf(out, ".L%s", label);
#else
    fprintf(out, "L%s", label);
#endif

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
    emit_asmoper(out, mov->src, OS_DWORD);
    fprintf(out, ", ");
    emit_asmoper(out, mov->dst, OS_DWORD);
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
    emit_asmoper(out, unary->arg, OS_DWORD);
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
        case BOP_LSHIFT:        opcode = "shl"; break;
        case BOP_RSHIFT:        opcode = "sar"; break;
        case BOP_BITAND:        opcode = "and"; break;
        case BOP_BITOR:         opcode = "or"; break;
        case BOP_BITXOR:        opcode = "xor"; break;

        //
        // NOTE idiv is handled as a special case.
        //
        default:
            ICE_ASSERT(((void)"invalid binary opcode in emit_binary", false));
    }

    fprintf(out, "        %sl ", opcode);

    if (binary->op == BOP_RSHIFT || binary->op == BOP_LSHIFT) {
        emit_asmoper(out, binary->src, OS_BYTE);
    } else {
        emit_asmoper(out, binary->src, OS_DWORD);
    }
    fprintf(out, ", ");
    emit_asmoper(out, binary->dst, OS_DWORD);
    fprintf(out, "\n");
}

//
// Emit an IDIV instruction.
//
static void emit_idiv(FILE *out, AsmIdiv *idiv)
{
    fprintf(out, "        idivl ");
    emit_asmoper(out, idiv->arg, OS_DWORD);
    fprintf(out, "\n");
}

//
// Emit a CMP instruction.
//
static void emit_cmp(FILE *out, AsmCmp *cmp)
{
    fprintf(out, "        cmpl ");
    emit_asmoper(out, cmp->left, OS_DWORD);
    fprintf(out, ", ");
    emit_asmoper(out, cmp->right, OS_DWORD);
    fprintf(out, "\n");
}

//
// Emit a JMP instruction.
//
static void emit_jump(FILE *out, AsmJump *jump)
{
    fprintf(out, "        jmp ");
    emit_label(out, jump->target);
    fprintf(out, "\n");
}

//
// Emit a JMPCC instruction.
//
static void emit_jumpcc(FILE *out, AsmJumpCc *jumpcc)
{
    fprintf(out, "        j%s ", acc_describe(jumpcc->cc));
    emit_label(out, jumpcc->target);
    fprintf(out, "\n");
}

//
// Emit a SETCC instruction.
//
static void emit_setcc(FILE *out, AsmSetCc *setcc)
{
    fprintf(out, "        set%s ", acc_describe(setcc->cc));
    emit_asmoper(out, setcc->dst, OS_BYTE);
    fprintf(out, "\n");
}

//
// Emit a label.
//
static void emit_label_instr(FILE *out, AsmLabel *label)
{
    emit_label(out, label->label);
    fprintf(out, ":\n");
}

//
// Emit code to reserve locals on the stack.
//
static void emit_stack_reserve(FILE *out, AsmStackReserve *reserve)
{
    fprintf(out, "        subq $%d, %%rsp\n", reserve->bytes);
}

//
// Emit code to free locals on the stack.
//
static void emit_stack_free(FILE *out, AsmStackFree *reserve)
{
    fprintf(out, "        addq $%d, %%rsp\n", reserve->bytes);
}

//
// Emit a push.
//
static void emit_push(FILE *out, AsmPush *push)
{
    fprintf(out, "        push ");
    emit_asmoper(out, push->value, OS_QWORD);
    fprintf(out, "\n");
}

//
// Emit a call.
//
static void emit_call(FILE *out, AsmCall *call)
{
#ifdef __APPLE__
    fprintf(out, "        call _%s\n", call->id);
#else
    fprintf(out, "        call %s\n", call->id);
#endif
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
static void emit_program(FILE *out, AsmProgram *prog)
{
    for (ListNode *curr = prog->funcs.head; curr; curr = curr->next) {
        AsmNode *func = CONTAINER_OF(curr, AsmNode, list);
        emitcode_recurse(out, func, &func->loc);        
    }

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
#ifndef __APPLE__
        // 
        // TODO figure out why Mac assembler doesn't like this
        // line number format.
        //
        fprintf(out, "# %s\n", sloc);
#endif
        safe_free(sloc);
    }

    switch (node->tag) {
        case ASM_PROG:          emit_program(out, &node->prog); break;
        case ASM_FUNC:          emit_function(out, &node->func, loc); break;
        case ASM_STACK_RESERVE: emit_stack_reserve(out, &node->stack_reserve); break;
        case ASM_STACK_FREE:    emit_stack_free(out, &node->stack_free); break;
        case ASM_MOV:           emit_mov(out, &node->mov); break;
        case ASM_UNARY:         emit_unary(out, &node->unary); break;
        case ASM_BINARY:        emit_binary(out, &node->binary); break;
        case ASM_CMP:           emit_cmp(out, &node->cmp); break;
        case ASM_JUMP:          emit_jump(out, &node->jump); break;
        case ASM_JUMPCC:        emit_jumpcc(out, &node->jumpcc); break;
        case ASM_SETCC:         emit_setcc(out, &node->setcc); break;
        case ASM_LABEL:         emit_label_instr(out, &node->label); break;
        case ASM_RET:           emit_ret(out); break;
        case ASM_CDQ:           emit_cdq(out); break;
        case ASM_IDIV:          emit_idiv(out, &node->idiv); break;
        case ASM_PUSH:          emit_push(out, &node->push); break;
        case ASM_CALL:          emit_call(out, &node->call); break;
    }
}

//
// Top level entry to emit code.
//
void emitcode(FILE *out, AsmNode *prog)
{
    FileLine loc = { NULL, 0 };
    emitcode_recurse(out, prog, &loc);
}
