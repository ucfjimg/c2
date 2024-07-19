#include "asm-ast.h"

#include "ice.h"
#include "operators.h"
#include "safemem.h"

#include <stdio.h>

static void asm_print_recurse(AsmNode *node, FileLine *loc, bool locs);

//
// Return a register name. The returned string is static, not allocated.
//
char *reg_name(Register reg)
{
    switch (reg) {
        case REG_RAX:   return "rax";
        case REG_R10:   return "r10";
    }

    return "<invalid-reg>";
}

//
// Clone an assembly operand.
//
AsmOperand *aoper_clone(AsmOperand *oper)
{
    switch (oper->tag) {
        case AOP_IMM:       return aoper_imm(oper->imm);
        case AOP_REG:       return aoper_reg(oper->reg);
        case AOP_PSEUDOREG: return aoper_pseudoreg(oper->pseudoreg);
        case AOP_STACK:     return aoper_stack(oper->stack_offset);

    default:
        ICE_ASSERT(((void)"invalid operand in aoper_clone", false));
    }

    //
    // never reached.
    //
    return NULL;
}

//
// Construct an assembly operand of a register.
//
AsmOperand *aoper_reg(Register reg)
{
    AsmOperand *op = safe_zalloc(sizeof(AsmOperand));

    op->tag = AOP_REG;
    op->reg = reg;

    return op;
}

//
// Constuct an assembly operand of a pseudo-register.
//
AsmOperand *aoper_pseudoreg(char *name)
{
    AsmOperand *op = safe_zalloc(sizeof(AsmOperand));

    op->tag = AOP_PSEUDOREG;
    op->pseudoreg = safe_strdup(name);

    return op;
}

//
// Construct an assembly operand of an immdiate.
//
AsmOperand *aoper_imm(unsigned long val)
{
    AsmOperand *op = safe_zalloc(sizeof(AsmOperand));

    op->tag = AOP_IMM;
    op->imm = val;
    return op;
}

//
// Allocate a stack operaand. The offset is a (negative) offset from 
// the frame pointer RBP.
//
AsmOperand *aoper_stack(int offset)
{
    AsmOperand *op = safe_zalloc(sizeof(AsmOperand));

    op->tag = AOP_STACK;
    op->stack_offset = offset;
    return op;
}

//
// Free an assembly operand.
//
void aoper_free(AsmOperand *op)
{
    if (op) {
        switch (op->tag) { 
            case AOP_PSEUDOREG: safe_free(op->pseudoreg); break;

            default:
                break;
        }
        safe_free(op);
    }
}

//
// Print an assembly operand, with no newline.
//
void aoper_print(AsmOperand *op)
{
    switch (op->tag) {
        case AOP_IMM: printf("%lu", op->imm); break;
        case AOP_REG: printf("$%s", reg_name(op->reg)); break;
        case AOP_PSEUDOREG: printf("%s", op->pseudoreg); break;
        case AOP_STACK: printf("[$rbp%d]", op->stack_offset); break;
    }
}

//
// Construct an empty assembly program.
//
AsmNode *asm_prog(FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_PROG;
    node->loc = loc;
    node->prog.func = NULL;

    return node;
}

//
// Construct an assembly function with no instructions.
//
AsmNode *asm_func(char *name, List body, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_FUNC;
    node->loc = loc;
    node->func.name = safe_strdup(name);
    node->func.body = body;

    return node;
}

//
// Construct an assembly mov instruction.
//
AsmNode *asm_mov(AsmOperand *src, AsmOperand *dst, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_MOV;
    node->loc = loc;
    node->mov.src = src;
    node->mov.dst = dst;

    return node;
}

//
// Construct an assembly unary operator instruction.
//
AsmNode *asm_unary(UnaryOp op, AsmOperand *arg, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_UNARY;
    node->loc = loc;
    node->unary.op = op;
    node->unary.arg = arg;

    return node;
}


//
// Construct a return instruction.
//
AsmNode *asm_ret(FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_RET;
    node->loc = loc;

    return node;
}

//
// Construct a stack reserve instruction. `bytes` is the number of bytes to
// allocate in the stack frame for local variables.
//
AsmNode *asm_stack_reserve(int bytes, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_STACK_RESERVE;
    node->loc = loc;
    node->stack_reserve.bytes = bytes;

    return node;
}

//
// Free an assembly program
//
void asm_prog_free(AsmProgram *prog)
{
    asm_free(prog->func);
}

//
// Free an assembly mov instruction.
//
void asm_mov_free(AsmMov *mov)
{
    aoper_free(mov->src);
    aoper_free(mov->dst);
}

//
// Free an assembly unary instruction.
//
void asm_unary_free(AsmUnary *unary)
{
    aoper_free(unary->arg);
}

//
// Free an assembly function.
//
void asm_func_free(AsmFunction *func)
{
    safe_free(func->name);

    for (ListNode *curr = func->body.head; curr; ) {
        ListNode *next = curr->next;

        AsmNode *node = CONTAINER_OF(curr, AsmNode, list);
        asm_free(node);
        curr = next;
    }
}

//
// Free an assembly node.
//
void asm_free(AsmNode *node)
{
    if (node) {
        switch (node->tag) {
            case ASM_PROG:  asm_prog_free(&node->prog); break;
            case ASM_MOV:   asm_mov_free(&node->mov); break;
            case ASM_UNARY: asm_unary_free(&node->unary); break;
            case ASM_FUNC:  asm_func_free(&node->func); break; 

            default:
                break;
        }

        safe_free(node);
    }
}

//
// Print an entire assembly program.
//
static void asm_prog_print(AsmProgram *prog, FileLine *loc, bool locs)
{
    printf("#\n# program\n#\n");
    asm_print_recurse(prog->func, loc, locs);
}

//
// Print an assembly function.
//
static void asm_func_print(AsmFunction *func, FileLine *loc, bool locs)
{
    printf("%s:\n", func->name);

    for (ListNode *curr = func->body.head; curr; curr = curr->next) {
        AsmNode *node = CONTAINER_OF(curr, AsmNode, list);
        asm_print_recurse(node, loc, locs);
    }
}

//
// Print a mov instruction.
//
static void asm_mov_print(AsmMov *mov)
{
    printf("        mov ");
    aoper_print(mov->src);
    printf(" => ");
    aoper_print(mov->dst);
    printf("\n");
}

//
// Print a unary operator instruction.
//
static void asm_unary_print(AsmUnary *unary)
{
    printf("        uop(%s) ", uop_describe(unary->op));
    aoper_print(unary->arg);
    printf("\n");
}

//
// Print a ret instruction.
//
static void asm_ret_print(void)
{
    printf("        ret\n");
}

//
// Print a stack reserve instruction.
//
static void asm_stack_reserve_print(AsmStackReserve *reserve)
{
    printf("        stack-reserve %d\n", reserve->bytes);
}

//
// Print the assembly AST.
//
static void asm_print_recurse(AsmNode *node, FileLine *loc, bool locs)
{
    if (locs) {
        if (node->loc.fname != loc->fname || node->loc.line != loc->line) {
            *loc = node->loc;
            char *sloc = fileline_describe(loc);
            printf("# %s\n", sloc);
            safe_free(sloc);
        }
    }

    switch (node->tag) {
        case ASM_PROG:          asm_prog_print(&node->prog, loc, locs); break;
        case ASM_FUNC:          asm_func_print(&node->func, loc, locs); break;
        case ASM_MOV:           asm_mov_print(&node->mov); break;
        case ASM_UNARY:         asm_unary_print(&node->unary); break;
        case ASM_RET:           asm_ret_print(); break;
        case ASM_STACK_RESERVE: asm_stack_reserve_print(&node->stack_reserve); break;
    }
}

//
// Top level entry to printing the assembly AST.
// 
void asm_print(AsmNode *node, bool locs)
{
    FileLine loc = { NULL, 0 };

    asm_print_recurse(node, &loc, locs);
}


