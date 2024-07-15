#include "asm-ast.h"

#include "safemem.h"

#include <stdio.h>

static void asm_print_recurse(AsmNode *node, FileLine loc, bool locs);


//
// Return a register name. The returned string is static, not allocated.
//
char *reg_name(Register reg)
{
    switch (reg) {
        case REG_RAX:   return "rax";
    }

    return "<invalid-reg>";
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
// Free an assembly operand.
//
void aoper_free(AsmOperand *op)
{
    safe_free(op);
}

//
// Print an assembly operand, with no newline.
//
void aoper_print(AsmOperand *op)
{
    switch (op->tag) {
        case AOP_IMM: printf("%lu", op->imm); break;
        case AOP_REG: printf("%s", reg_name(op->reg)); break;
    }
}

//
// Construct an empty assembly program.
//
AsmNode *asm_prog(void)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->prog.func = NULL;

    return node;
}

//
// Construct an assembly function with no instructions.
//
AsmNode *asm_func(char *name)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_FUNC;
    node->func.name = safe_strdup(name);

    return node;
}

//
// Construct an assembly mov instruction.
//
AsmNode *asm_mov(AsmOperand *src, AsmOperand *dst)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_MOV;
    node->mov.src = src;
    node->mov.dst = dst;

    return node;
}

//
// Construct a return instruction.
//
AsmNode *asm_ret(void)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_RET;

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
            case ASM_FUNC:  asm_func_free(&node->func); break; 
       }

        safe_free(node);
    }
}

//
// Print an entire assembly program.
//
static void asm_prog_print(AsmProgram *prog, FileLine loc, bool locs)
{
    printf("#\n# program\n#\n");
    asm_print_recurse(prog->func, loc, locs);
}

//
// Print an assembly function.
//
static void asm_func_print(AsmFunction *func, FileLine loc, bool locs)
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
// Print a ret instruction.
//
static void asm_ret_print(void)
{
    printf("        ret\n");
}

//
// Print the assembly AST.
//
static void asm_print_recurse(AsmNode *node, FileLine loc, bool locs)
{
    if (locs) {
        if (node->loc.fname != loc.fname || node->loc.line != loc.line) {
            printf("# %s:%d\n", node->loc.fname, node->loc.line);
            loc = node->loc;
        }
    }

    switch (node->tag) {
        case ASM_PROG: asm_prog_print(&node->prog, loc, locs); break;
        case ASM_FUNC: asm_func_print(&node->func, loc, locs); break;
        case ASM_MOV:  asm_mov_print(&node->mov); break;
        case ASM_RET:  asm_ret_print(); break;
    }
}

//
// Top level entry to printing the assembly AST.
// 
void asm_print(AsmNode *node, bool locs)
{
    FileLine loc = { NULL, 0 };

    asm_print_recurse(node, loc, locs);
}


