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
        case REG_RCX:   return "rcx";
        case REG_RDX:   return "rdx";
        case REG_R10:   return "r10";
        case REG_R11:   return "r11";
    }

    return "<invalid-reg>";
}

//
// Return a condition code description as a static string.
//
const char *acc_describe(AsmConditionCode cc)
{
    switch (cc) {
        case ACC_E:     return "E";
        case ACC_NE:    return "NE";
        case ACC_G:     return "G";
        case ACC_GE:    return "GE";
        case ACC_L:     return "L";
        case ACC_LE:    return "LE";
    }

    return "<invalid-condition-code>";
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
// Construct an assembly binary operator instruction.
//
AsmNode *asm_binary(BinaryOp op, AsmOperand *src, AsmOperand *dst, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_BINARY;
    node->loc = loc;
    node->binary.op = op;
    node->binary.src = src;
    node->binary.dst = dst;

    return node;
}

//
// Construct an assembly compare instruction.
//
AsmNode *asm_cmp(AsmOperand *left, AsmOperand *right, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_CMP;
    node->loc = loc;
    node->cmp.left = left;
    node->cmp.right = right;

    return node;
}

//
// Construct an assembly unconditional jump instruction.
//
AsmNode *asm_jump(char *target, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_JUMP;
    node->loc = loc;
    node->jump.target = safe_strdup(target);

    return node;
}

//
// Construct an assembly conditional jump instruction.
//
AsmNode *asm_jumpcc(char *target, AsmConditionCode cc, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_JUMPCC;
    node->loc = loc;
    node->jumpcc.target = safe_strdup(target);
    node->jumpcc.cc = cc;

    return node;
}

//
// Construct an assembly label.
//
AsmNode *asm_label(char *label, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_LABEL;
    node->loc = loc;
    node->label.label = safe_strdup(label);

    return node;
}

//
// Construct an assembly set on condition instruction.
//
AsmNode *asm_setcc(AsmOperand *dst, AsmConditionCode cc, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_SETCC;
    node->loc = loc;
    node->setcc.dst = dst;
    node->setcc.cc = cc;

    return node;
}

//
// Construct an assembly idiv (signed division) instruction.
//
AsmNode *asm_idiv(AsmOperand *arg, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_IDIV;
    node->loc = loc;
    node->idiv.arg = arg;

    return node;
}

//
// Construct a cdq (convert doubleword to quadword) instruction.
//
AsmNode *asm_cdq(FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_CDQ;
    node->loc = loc;

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
static void asm_prog_free(AsmProgram *prog)
{
    asm_free(prog->func);
}

//
// Free an assembly mov instruction.
//
static void asm_mov_free(AsmMov *mov)
{
    aoper_free(mov->src);
    aoper_free(mov->dst);
}

//
// Free an assembly unary instruction.
//
static void asm_unary_free(AsmUnary *unary)
{
    aoper_free(unary->arg);
}

//
// Free an assembly binary instruction.
//
static void asm_binary_free(AsmBinary *binary)
{
    aoper_free(binary->src);
    aoper_free(binary->dst);
}

//
// Free an assembly compare instruction.
//
static void asm_cmp_free(AsmCmp *cmp)
{
    aoper_free(cmp->left);
    aoper_free(cmp->right);
}

//
// Free an assembly jump instruction.
//
static void asm_jump_free(AsmJump *jump)
{
    safe_free(jump->target);
}

//
// Free an assembly conditional jump instruction.
//
static void asm_jumpcc_free(AsmJumpCc *jumpcc)
{
    safe_free(jumpcc->target);
}

//
// Free an assembly label.
//
static void asm_label_free(AsmLabel *label)
{
    safe_free(label->label);
}

//
// Free an assembly set on condition instruction.
//
static void asm_setcc_free(AsmSetCc *setcc)
{
    aoper_free(setcc->dst);
}

//
// Free an assembly idiv instruction.
//
static void asm_idiv_free(AsmIdiv *idiv)
{
    aoper_free(idiv->arg);
}

//
// Free an assembly function.
//
static void asm_func_free(AsmFunction *func)
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
            case ASM_PROG:      asm_prog_free(&node->prog); break;
            case ASM_MOV:       asm_mov_free(&node->mov); break;
            case ASM_UNARY:     asm_unary_free(&node->unary); break;
            case ASM_BINARY:    asm_binary_free(&node->binary); break;
            case ASM_CMP:       asm_cmp_free(&node->cmp); break;
            case ASM_JUMP:      asm_jump_free(&node->jump); break;
            case ASM_JUMPCC:    asm_jumpcc_free(&node->jumpcc); break;
            case ASM_LABEL:     asm_label_free(&node->label); break;
            case ASM_SETCC:     asm_setcc_free(&node->setcc); break;
            case ASM_IDIV:      asm_idiv_free(&node->idiv); break;
            case ASM_FUNC:      asm_func_free(&node->func); break; 

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
// Print a binary operator instruction.
//
static void asm_binary_print(AsmBinary *binary)
{
    printf("        bop(%s) ", bop_describe(binary->op));
    aoper_print(binary->src);
    printf(" => ");
    aoper_print(binary->dst);
    printf("\n");
}

//
// Print a compare instruction.
//
static void asm_cmp_print(AsmCmp *cmp)
{
    printf("        cmp ");
    aoper_print(cmp->left);
    printf(", ");
    aoper_print(cmp->right);
    printf("\n");
}

//
// Print a jump instruction.
//
static void asm_jump_print(AsmJump *jump)
{
    printf("        jump %s\n", jump->target);
}

//
// Print a conditional set instruction.
//
static void asm_setcc_print(AsmSetCc *setcc)
{
    printf("        set%s ", acc_describe(setcc->cc));
    aoper_print(setcc->dst);
    printf("\n");
}

//
// Print a conditional jump instruction.
//
static void asm_jumpcc_print(AsmJumpCc *jumpcc)
{
    printf("        jump%s %s\n", acc_describe(jumpcc->cc), jumpcc->target);
}

//
// Print a label.
//
static void asm_label_print(AsmLabel *label)
{
    printf("%s:\n", label->label);
}

//
// Print an idiv instruction.
//
static void asm_idiv_print(AsmIdiv *idiv)
{
    printf("        idiv ");
    aoper_print(idiv->arg);
    printf("\n");
}

//
// Print a cdq instruction.
//
static void asm_cdq_print(void)
{
    printf("        cdq\n");
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
        case ASM_BINARY:        asm_binary_print(&node->binary); break;
        case ASM_CMP:           asm_cmp_print(&node->cmp); break;
        case ASM_JUMP:          asm_jump_print(&node->jump); break;
        case ASM_JUMPCC:        asm_jumpcc_print(&node->jumpcc); break;
        case ASM_LABEL:         asm_label_print(&node->label); break;
        case ASM_SETCC:         asm_setcc_print(&node->setcc); break;
        case ASM_IDIV:          asm_idiv_print(&node->idiv); break;
        case ASM_CDQ:           asm_cdq_print(); break;
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


