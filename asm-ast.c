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
        case REG_RDI:   return "rdi";
        case REG_RSI:   return "rsi";
        case REG_R8:    return "r8";
        case REG_R9:    return "r9";
        case REG_R10:   return "r10";
        case REG_R11:   return "r11";
        case REG_RSP:   return "rsp";
        case REG_RBP:   return "rbp";
        case REG_XMM0:  return "xmm0";
        case REG_XMM1:  return "xmm1";
        case REG_XMM2:  return "xmm2";
        case REG_XMM3:  return "xmm3";
        case REG_XMM4:  return "xmm4";
        case REG_XMM5:  return "xmm5";
        case REG_XMM6:  return "xmm6";
        case REG_XMM7:  return "xmm7";
        case REG_XMM8:  return "xmm8";
        case REG_XMM9:  return "xmm9";
        case REG_XMM10: return "xmm10";
        case REG_XMM11: return "xmm11";
        case REG_XMM12: return "xmm12";
        case REG_XMM13: return "xmm13";
        case REG_XMM14: return "xmm14";
        case REG_XMM15: return "xmm15";
    }

    return "<invalid-reg>";
}

//
// Return true if the given register is an XMM register.
//
bool is_xmm(Register reg)
{
    return reg >= REG_XMM0 && reg <= REG_XMM15;
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
        case ACC_A:     return "A";
        case ACC_AE:    return "AE";
        case ACC_B:     return "B";
        case ACC_BE:    return "BE";
    }

    return "<invalid-condition-code>";
}

//
// Allocate an assembly type of a longword.
//
AsmType *asmtype_long(void)
{
    AsmType *at = safe_zalloc(sizeof(AsmType));
    at->tag = AT_LONGWORD;
    return at;
}

//
// Allocate an assembly type of a quadword.
//
AsmType *asmtype_quad(void)
{
    AsmType *at = safe_zalloc(sizeof(AsmType));
    at->tag = AT_QUADWORD;
    return at;
}

//
// Allocate an assembly type of a double.
//
AsmType *asmtype_double(void)
{
    AsmType *at = safe_zalloc(sizeof(AsmType));
    at->tag = AT_DOUBLE;
    return at;
}

//
// Free an assembly type object.
//
void asmtype_free(AsmType *type)
{
    safe_free(type);
}

//
// Clone an assembly type object.
//
AsmType *asmtype_clone(AsmType *type)
{
    switch (type->tag) {
        case AT_LONGWORD:       return asmtype_long();
        case AT_QUADWORD:       return asmtype_quad();
        case AT_DOUBLE:         return asmtype_double();
    }

    ICE_ASSERT(((void)"invalid assembly type in asmtype_clone", false));
    return asmtype_long();
}

//
// Return an allocated string describing an assembly type.
//
char *asmtype_describe(AsmType *at)
{
    switch (at->tag) {
        case AT_LONGWORD: return saprintf("longword");
        case AT_QUADWORD: return saprintf("quadword");
        case AT_DOUBLE:   return saprintf("double");
    }

    ICE_ASSERT(((void)"invalid assembly type in asmtype_describe", false));
    return safe_strdup("");
}

//
// Return the size of an assembly type. 
//
int asmtype_size(AsmType *type)
{
    switch (type->tag) {
        case AT_LONGWORD: return 4;
        case AT_QUADWORD: return 8;
        case AT_DOUBLE:   return 8;
    }

    ICE_ASSERT(((void)"invalid assembly type in asmtype_size", false));
    return 1;
}

//
// Return the alignment of an assembly type. 
//
int asmtype_alignment(AsmType *type)
{
    switch (type->tag) {
        case AT_LONGWORD: return 4;
        case AT_QUADWORD: return 8;
        case AT_DOUBLE:   return 8;
    }

    ICE_ASSERT(((void)"invalid assembly type in asmtype_alignment", false));
    return 1;
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
        case AOP_DATA:      return aoper_data(oper->data);
        case AOP_MEMORY:    return aoper_memory(oper->memory.reg, oper->memory.offset);
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
// Allocate a memory operand. The offset is a (negative) offset from 
// the given register.
//
AsmOperand *aoper_memory(Register reg, int offset)
{
    AsmOperand *op = safe_zalloc(sizeof(AsmOperand));

    op->tag = AOP_MEMORY;
    op->memory.reg = reg;
    op->memory.offset = offset;
    return op;
}

//
// Allocate a static operand.
//
AsmOperand *aoper_data(char *name)
{
    AsmOperand *op = safe_zalloc(sizeof(AsmOperand));

    op->tag = AOP_DATA;
    op->data = safe_strdup(name);
    return op;    
}

//
// Check if an assembly operand is a memory reference.
//
bool aoper_is_mem(AsmOperand *oper)
{
    return oper->tag == AOP_MEMORY || oper->tag == AOP_DATA;
}

//
// Free an assembly operand.
//
void aoper_free(AsmOperand *op)
{
    if (op) {
        switch (op->tag) { 
            case AOP_PSEUDOREG: safe_free(op->pseudoreg); break;
            case AOP_DATA: safe_free(op->data); break;

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
        case AOP_MEMORY: printf("[%s+%d]", reg_name(op->memory.reg), op->memory.offset); break;
        case AOP_DATA: printf("%s", op->data); break;
    }
}

//
// Construct an empty assembly program.
//
AsmNode *asm_prog(List funcs, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_PROG;
    node->loc = loc;
    node->prog.funcs = funcs;

    return node;
}

//
// Construct an assembly function.
//
AsmNode *asm_func(char *name, List body, bool global, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_FUNC;
    node->loc = loc;
    node->func.name = safe_strdup(name);
    node->func.body = body;
    node->func.global = global;

    return node;
}

//
// Construct an assembly static variable.
//
AsmNode *asm_static_var(char *name, bool global, int alignment, Const init, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_STATIC_VAR;
    node->loc = loc;
    node->static_var.name = safe_strdup(name);
    node->static_var.global = global;
    node->static_var.alignment = alignment;
    node->static_var.init = init;

    return node;    
}

//
// Construct an assembly static const.
//
AsmNode *asm_static_const(char *name, int alignment, Const init, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_STATIC_CONST;
    node->loc = loc;
    node->static_const.name = safe_strdup(name);
    node->static_const.alignment = alignment;
    node->static_const.init = init;

    return node;    
}

//
// Construct an assembly mov instruction.
//
AsmNode *asm_mov(AsmOperand *src, AsmOperand *dst, AsmType *type, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_MOV;
    node->loc = loc;
    node->mov.src = src;
    node->mov.dst = dst;
    node->mov.type = type;

    return node;
}

//
// Construct an assembly movsx instruction.
//
AsmNode *asm_movsx(AsmOperand *src, AsmOperand *dst, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_MOVSX;
    node->loc = loc;
    node->movsx.src = src;
    node->movsx.dst = dst;

    return node;
}

//
// Construct an assembly movzx instruction.
//
AsmNode *asm_movzx(AsmOperand *src, AsmOperand *dst, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_MOVZX;
    node->loc = loc;
    node->movzx.src = src;
    node->movzx.dst = dst;

    return node;
}

//
// Construct an assembly lea instruction.
//
AsmNode *asm_lea(AsmOperand *src, AsmOperand *dst, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_LEA;
    node->loc = loc;
    node->lea.src = src;
    node->lea.dst = dst;

    return node;
}
//
// Construct an assembly unary operator instruction.
//
AsmNode *asm_unary(UnaryOp op, AsmOperand *arg, AsmType *type, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_UNARY;
    node->loc = loc;
    node->unary.op = op;
    node->unary.arg = arg;
    node->unary.type = type;

    return node;
}

//
// Construct an assembly binary operator instruction.
//
AsmNode *asm_binary(BinaryOp op, AsmOperand *src, AsmOperand *dst, AsmType *type, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_BINARY;
    node->loc = loc;
    node->binary.op = op;
    node->binary.src = src;
    node->binary.dst = dst;
    node->binary.type = type;

    return node;
}

//
// Construct an assembly compare instruction.
//
AsmNode *asm_cmp(AsmOperand *left, AsmOperand *right, AsmType *type, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_CMP;
    node->loc = loc;
    node->cmp.left = left;
    node->cmp.right = right;
    node->cmp.type = type;

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
AsmNode *asm_idiv(AsmOperand *arg, AsmType *type, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_IDIV;
    node->loc = loc;
    node->idiv.arg = arg;
    node->idiv.type = type;

    return node;
}

//
// Construct an assembly div (unsigned division) instruction.
//
AsmNode *asm_div(AsmOperand *arg, AsmType *type, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_DIV;
    node->loc = loc;
    node->div.arg = arg;
    node->div.type = type;

    return node;
}

//
// Construct a cdq (convert doubleword to quadword) instruction.
//
AsmNode *asm_cdq(AsmType *type, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_CDQ;
    node->loc = loc;
    node->cdq.type = type;

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
// Construct a stack free instruction, to remove `bytes` bytes from 
// the stack.
//
AsmNode *asm_stack_free(int bytes, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_STACK_FREE;
    node->loc = loc;
    node->stack_free.bytes = bytes;

    return node;
}

//
// Construct a push instruction.
//
AsmNode *asm_push(AsmOperand *value, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_PUSH;
    node->loc = loc;
    node->push.value = value;

    return node;
}

//
// Construct a call instruction.
//
AsmNode *asm_call(char *id, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_CALL;
    node->loc = loc;
    node->call.id = safe_strdup(id);

    return node;
}

//
// Construct a double to integer conversion instruction.
//
AsmNode *asm_cvttsd2si(AsmOperand *src, AsmOperand *dst, AsmType *type, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_CVTTSD2SI;
    node->loc = loc;
    node->cvttsd2si.src = src;
    node->cvttsd2si.dst = dst;
    node->cvttsd2si.type = type;

    return node;
}

//
// Construct an integer to double coversion instruction.
//
AsmNode *asm_cvtsi2sd(AsmOperand *src, AsmOperand *dst, AsmType *type, FileLine loc)
{
    AsmNode *node = safe_zalloc(sizeof(AsmNode));

    node->tag = ASM_CVTSI2SD;
    node->loc = loc;
    node->cvtsi2sd.src = src;
    node->cvtsi2sd.dst = dst;
    node->cvtsi2sd.type = type;

    return node;

}

//
// Free an assembly program
//
static void asm_prog_free(AsmProgram *prog)
{
    for (ListNode *curr = prog->funcs.head; curr; ) {
        ListNode *next = curr->next;
        AsmNode *func = CONTAINER_OF(curr, AsmNode, list);
        asm_free(func);
        curr = next;
    }
}

//
// Free an assembly mov instruction.
//
static void asm_mov_free(AsmMov *mov)
{
    aoper_free(mov->src);
    aoper_free(mov->dst);
    asmtype_free(mov->type);
}

//
// Free an assembly movsx instruction.
//
static void asm_movsx_free(AsmMovsx *movsx)
{
    aoper_free(movsx->src);
    aoper_free(movsx->dst);
}

//
// Free an assembly movzx instruction.
//
static void asm_movzx_free(AsmMovsx *movzx)
{
    aoper_free(movzx->src);
    aoper_free(movzx->dst);
}

//
// Free an assembly lea instruction.
//
static void asm_lea_free(AsmLea *lea)
{
    aoper_free(lea->src);
    aoper_free(lea->dst);
}

//
// Free an assembly unary instruction.
//
static void asm_unary_free(AsmUnary *unary)
{
    aoper_free(unary->arg);
    asmtype_free(unary->type);
}

//
// Free an assembly binary instruction.
//
static void asm_binary_free(AsmBinary *binary)
{
    aoper_free(binary->src);
    aoper_free(binary->dst);
    asmtype_free(binary->type);
}

//
// Free an assembly compare instruction.
//
static void asm_cmp_free(AsmCmp *cmp)
{
    aoper_free(cmp->left);
    aoper_free(cmp->right);
    asmtype_free(cmp->type);
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
    asmtype_free(idiv->type);
}

//
// Free an assembly div instruction.
//
static void asm_div_free(AsmDiv *div)
{
    aoper_free(div->arg);
    asmtype_free(div->type);
}

//
// Free an assembly cdq instruction.
//
static void asm_cdq_free(AsmCdq *cdq)
{
    asmtype_free(cdq->type);
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
// Free an assembly static variable.
//
static void asm_static_var_free(AsmStaticVar *var)
{
    safe_free(var->name);
}

//
// Free an assembly static constant.
//
static void asm_static_const_free(AsmStaticConst *cn)
{
    safe_free(cn->name);
}

//
// Free a push instruction.
//
static void asm_push_free(AsmPush *push)
{
    aoper_free(push->value);
}

//
// Free a call instruction.
//
static void asm_call_free(AsmCall *call)
{
    safe_free(call->id);
}

//
// Free a cvttsd2si instruction.
//
static void asm_cvttsd2si_free(AsmCvttsd2si *cvt)
{
    aoper_free(cvt->src);
    aoper_free(cvt->dst);
    asmtype_free(cvt->type);
}

//
// Free a cvtsi2sd instruction.
//
static void asm_cvtsi2sd_free(AsmCvtsi2sd *cvt)
{
    aoper_free(cvt->src);
    aoper_free(cvt->dst);
    asmtype_free(cvt->type);
}

//
// Free an assembly node.
//
void asm_free(AsmNode *node)
{
    if (node) {
        switch (node->tag) {
            case ASM_PROG:          asm_prog_free(&node->prog); break;
            case ASM_MOV:           asm_mov_free(&node->mov); break;
            case ASM_MOVSX:         asm_movsx_free(&node->movsx); break;
            case ASM_MOVZX:         asm_movzx_free(&node->movsx); break;
            case ASM_LEA:           asm_lea_free(&node->lea); break;
            case ASM_UNARY:         asm_unary_free(&node->unary); break;
            case ASM_BINARY:        asm_binary_free(&node->binary); break;
            case ASM_CMP:           asm_cmp_free(&node->cmp); break;
            case ASM_JUMP:          asm_jump_free(&node->jump); break;
            case ASM_JUMPCC:        asm_jumpcc_free(&node->jumpcc); break;
            case ASM_LABEL:         asm_label_free(&node->label); break;
            case ASM_SETCC:         asm_setcc_free(&node->setcc); break;
            case ASM_IDIV:          asm_idiv_free(&node->idiv); break;
            case ASM_DIV:           asm_div_free(&node->div); break;
            case ASM_CDQ:           asm_cdq_free(&node->cdq); break;
            case ASM_FUNC:          asm_func_free(&node->func); break;
            case ASM_STATIC_VAR:    asm_static_var_free(&node->static_var); break; 
            case ASM_STATIC_CONST:  asm_static_const_free(&node->static_const); break; 
            case ASM_PUSH:          asm_push_free(&node->push); break;
            case ASM_CALL:          asm_call_free(&node->call); break;
            case ASM_CVTTSD2SI:     asm_cvttsd2si_free(&node->cvttsd2si); break;
            case ASM_CVTSI2SD:      asm_cvtsi2sd_free(&node->cvtsi2sd); break;

            case ASM_STACK_RESERVE: break;
            case ASM_STACK_FREE:    break;
            case ASM_RET:           break;
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
    for (ListNode *curr = prog->funcs.head; curr; curr = curr->next) {
        AsmNode *func = CONTAINER_OF(curr, AsmNode, list);
        asm_print_recurse(func, &func->loc, locs);
    }
}

//
// Print an assembly function.
//
static void asm_func_print(AsmFunction *func, FileLine *loc, bool locs)
{
    if (func->global) {
        printf(".global %s\n", func->name);
    }
    printf("%s:\n", func->name);

    for (ListNode *curr = func->body.head; curr; curr = curr->next) {
        AsmNode *node = CONTAINER_OF(curr, AsmNode, list);
        asm_print_recurse(node, loc, locs);
    }
}

//
// Print an assembly static variable.
//
static void asm_static_var_print(AsmStaticVar *var)
{
    if (var->global) {
        printf(".global %s\n", var->name);
    }

    printf(".align %d\n", var->alignment);
    
    if (var->init.tag == CON_FLOAT) {
        printf("%s: .double %.13g\n", var->name, var->init.floatval);
    } else if (var->init.intval.size == CIS_LONG) {
        printf("%s: .quad %lu\n", var->name, var->init.intval.value);
    } else {
        printf("%s: .long %lu\n", var->name, var->init.intval.value);
    }
}

//
// Print an assembly static constant.
//
static void asm_static_const_print(AsmStaticConst *cn)
{
    printf(".align %d\n", cn->alignment);

    if (cn->init.tag == CON_FLOAT) {
        printf("%s: .double %.13g\n", cn->name, cn->init.floatval);
    } else if (cn->init.intval.size == CIS_LONG) {
        printf("%s: .quad %lu\n", cn->name, cn->init.intval.value);
    } else {
        printf("%s: .long %lu\n", cn->name, cn->init.intval.value);
    }
}

//
// Print (without a newline) an assembly type.
//
static void asmtype_print(AsmType *at)
{
    char *desc = asmtype_describe(at);
    printf("%s", desc);
    safe_free(desc);
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
    printf(" # ");
    asmtype_print(mov->type);
    printf("\n");
}

//
// Print a movsx instruction.
//
static void asm_movsx_print(AsmMovsx *movsx)
{
    printf("        movsx ");
    aoper_print(movsx->src);
    printf(" => ");
    aoper_print(movsx->dst);
    printf("\n");
}

//
// Print a movzx instruction.
//
static void asm_movzx_print(AsmMovzx *movzx)
{
    printf("        movzx ");
    aoper_print(movzx->src);
    printf(" => ");
    aoper_print(movzx->dst);
    printf("\n");
}

//
// Print an lea instruction.
//
static void asm_lea_print(AsmLea *lea)
{
    printf("        lea ");
    aoper_print(lea->src);
    printf(" => ");
    aoper_print(lea->dst);
    printf("\n");
}

//
// Print a unary operator instruction.
//
static void asm_unary_print(AsmUnary *unary)
{
    printf("        uop(%s) ", uop_describe(unary->op));
    aoper_print(unary->arg);
    printf(" # ");
    asmtype_print(unary->type);
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
    printf(" # ");
    asmtype_print(binary->type);
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
    printf(" # ");
    asmtype_print(cmp->type);
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
    printf(" # ");
    asmtype_print(idiv->type);
    printf("\n");
}

//
// Print a div instruction.
//
static void asm_div_print(AsmDiv *div)
{
    printf("        div ");
    aoper_print(div->arg);
    printf(" # ");
    asmtype_print(div->type);
    printf("\n");
}

//
// Print a cdq instruction.
//
static void asm_cdq_print(AsmCdq *cdq)
{
    printf("        cdq # ");
    asmtype_print(cdq->type);
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
// Print a stack free instruction.
//
static void asm_stack_free_print(AsmStackFree *free)
{
    printf("        stack-free %d\n", free->bytes);
}

//
// Print a push instruction.
//
static void asm_push_print(AsmPush *push)
{
    printf("        push ");
    aoper_print(push->value);
    printf("\n");
}

//
// Print a call instruction.
//
static void asm_call_print(AsmCall *call)
{
    printf("        call %s\n", call->id);
}

//
// Print a cvttsd2si instruction.
//
static void asm_cvttsd2si_print(AsmCvttsd2si *cvt)
{
    printf("        cvttsd2si ");
    aoper_print(cvt->src);
    printf(" => ");
    aoper_print(cvt->dst);
    printf(" # ");
    asmtype_print(cvt->type);
    printf("\n");
}

//
// Print a cvtsi2sd instruction.
//
static void asm_cvtsi2sd_print(AsmCvtsi2sd *cvt)
{
    printf("        cvtsi2sd ");
    aoper_print(cvt->src);
    printf(" => ");
    aoper_print(cvt->dst);
    printf(" # ");
    asmtype_print(cvt->type);
    printf("\n");
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
        case ASM_STATIC_VAR:    asm_static_var_print(&node->static_var); break;
        case ASM_STATIC_CONST:  asm_static_const_print(&node->static_const); break;
        case ASM_MOV:           asm_mov_print(&node->mov); break;
        case ASM_MOVSX:         asm_movsx_print(&node->movsx); break;
        case ASM_MOVZX:         asm_movzx_print(&node->movzx); break;
        case ASM_LEA:           asm_lea_print(&node->lea); break;
        case ASM_UNARY:         asm_unary_print(&node->unary); break;
        case ASM_BINARY:        asm_binary_print(&node->binary); break;
        case ASM_CMP:           asm_cmp_print(&node->cmp); break;
        case ASM_JUMP:          asm_jump_print(&node->jump); break;
        case ASM_JUMPCC:        asm_jumpcc_print(&node->jumpcc); break;
        case ASM_LABEL:         asm_label_print(&node->label); break;
        case ASM_SETCC:         asm_setcc_print(&node->setcc); break;
        case ASM_IDIV:          asm_idiv_print(&node->idiv); break;
        case ASM_DIV:           asm_div_print(&node->div); break;
        case ASM_CDQ:           asm_cdq_print(&node->cdq); break;
        case ASM_RET:           asm_ret_print(); break;
        case ASM_STACK_RESERVE: asm_stack_reserve_print(&node->stack_reserve); break;
        case ASM_STACK_FREE:    asm_stack_free_print(&node->stack_free); break;
        case ASM_PUSH:          asm_push_print(&node->push); break;
        case ASM_CALL:          asm_call_print(&node->call); break;
        case ASM_CVTTSD2SI:     asm_cvttsd2si_print(&node->cvttsd2si); break;
        case ASM_CVTSI2SD:      asm_cvtsi2sd_print(&node->cvtsi2sd); break;
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


