#include "emitcode.h"

#include "fileline.h"
#include "ice.h"
#include "safemem.h"

typedef enum {
    OS_BYTE = 1,
    OS_DWORD = 4,
    OS_QWORD = 8,
} OperandSize;

typedef struct {
    char *suffix;               // suffix for instructions
    OperandSize size;           // size of object
} OperandSpec;

typedef struct {
    FILE *out;
    BackEndSymbolTable *bstab;
} EmitState;

static void emitcode_recurse(EmitState *state, AsmNode *node, FileLine *loc);

//
// Fill in an operand spec for the given assembly type.
//
static void type_to_operand_spec(AsmType *type, OperandSpec *spec)
{
    spec->suffix = "";
    spec->size = OS_DWORD;

    switch (type->tag) {
        case AT_LONGWORD:
            spec->suffix = "l";
            spec->size = OS_DWORD;
            break;

        case AT_QUADWORD:
            spec->suffix = "q";
            spec->size = OS_QWORD;
            break;

        case AT_DOUBLE:
            spec->suffix = "sd";
            spec->size = OS_QWORD;
            break;
    }
}

//
// Emit a register value.
//
static void emit_reg(EmitState *state, Register reg, OperandSize os)
{
    if (os == OS_QWORD) {
        switch (reg) {
            case REG_RAX:   fprintf(state->out, "%%rax"); break;
            case REG_RCX:   fprintf(state->out, "%%rcx"); break;
            case REG_RDX:   fprintf(state->out, "%%rdx"); break;
            case REG_RDI:   fprintf(state->out, "%%rdi"); break;
            case REG_RSI:   fprintf(state->out, "%%rsi"); break;
            case REG_R8:    fprintf(state->out, "%%r8"); break;
            case REG_R9:    fprintf(state->out, "%%r9"); break;
            case REG_R10:   fprintf(state->out, "%%r10"); break;
            case REG_R11:   fprintf(state->out, "%%r11"); break;
            case REG_RSP:   fprintf(state->out, "%%rsp"); break;
            case REG_RBP:   fprintf(state->out, "%%rbp"); break;
            case REG_XMM0:  fprintf(state->out, "%%xmm0"); break;
            case REG_XMM1:  fprintf(state->out, "%%xmm1"); break;
            case REG_XMM2:  fprintf(state->out, "%%xmm2"); break;
            case REG_XMM3:  fprintf(state->out, "%%xmm3"); break;
            case REG_XMM4:  fprintf(state->out, "%%xmm4"); break;
            case REG_XMM5:  fprintf(state->out, "%%xmm5"); break;
            case REG_XMM6:  fprintf(state->out, "%%xmm6"); break;
            case REG_XMM7:  fprintf(state->out, "%%xmm7"); break;
            case REG_XMM8:  fprintf(state->out, "%%xmm8"); break;
            case REG_XMM9:  fprintf(state->out, "%%xmm9"); break;
            case REG_XMM10: fprintf(state->out, "%%xmm10"); break;
            case REG_XMM11: fprintf(state->out, "%%xmm11"); break;
            case REG_XMM12: fprintf(state->out, "%%xmm12"); break;
            case REG_XMM13: fprintf(state->out, "%%xmm13"); break;
            case REG_XMM14: fprintf(state->out, "%%xmm14"); break;
            case REG_XMM15: fprintf(state->out, "%%xmm15"); break;
        }
    } else if (os == OS_DWORD) {
        switch (reg) {
            case REG_RAX:   fprintf(state->out, "%%eax"); break;
            case REG_RCX:   fprintf(state->out, "%%ecx"); break;
            case REG_RDX:   fprintf(state->out, "%%edx"); break;
            case REG_RDI:   fprintf(state->out, "%%edi"); break;
            case REG_RSI:   fprintf(state->out, "%%esi"); break;
            case REG_R8:    fprintf(state->out, "%%r8d"); break;
            case REG_R9:    fprintf(state->out, "%%r9d"); break;
            case REG_R10:   fprintf(state->out, "%%r10d"); break;
            case REG_R11:   fprintf(state->out, "%%r11d"); break;
            case REG_RSP:   fprintf(state->out, "%%esp"); break;
            case REG_RBP:   fprintf(state->out, "%%ebp"); break;
            case REG_XMM0:  
            case REG_XMM1:  
            case REG_XMM2:  
            case REG_XMM3:  
            case REG_XMM4:  
            case REG_XMM5:  
            case REG_XMM6:  
            case REG_XMM7:  
            case REG_XMM8:  
            case REG_XMM9:  
            case REG_XMM10: 
            case REG_XMM11: 
            case REG_XMM12: 
            case REG_XMM13: 
            case REG_XMM14: 
            case REG_XMM15: ICE_ASSERT(((void)"xmm registers have no 4-byte equivalent", false));
        }
    } else if (os == OS_BYTE) {
        switch (reg) {
            case REG_RAX: fprintf(state->out, "%%al"); break;
            case REG_RCX: fprintf(state->out, "%%cl"); break;
            case REG_RDX: fprintf(state->out, "%%dl"); break;
            case REG_RDI: fprintf(state->out, "%%dil"); break;
            case REG_RSI: fprintf(state->out, "%%sil"); break;
            case REG_R8:  fprintf(state->out, "%%r8b"); break;
            case REG_R9:  fprintf(state->out, "%%r9b"); break;
            case REG_R10: fprintf(state->out, "%%r10b"); break;
            case REG_R11: fprintf(state->out, "%%r11b"); break;
            case REG_RSP: ICE_ASSERT(((void)"cannot byte address rsp in emitcode.", false));
            case REG_RBP: ICE_ASSERT(((void)"cannot byte address rbp in emitcode.", false));
            case REG_XMM0:  
            case REG_XMM1:  
            case REG_XMM2:  
            case REG_XMM3:  
            case REG_XMM4:  
            case REG_XMM5:  
            case REG_XMM6:  
            case REG_XMM7:  
            case REG_XMM8:  
            case REG_XMM9:  
            case REG_XMM10: 
            case REG_XMM11: 
            case REG_XMM12: 
            case REG_XMM13: 
            case REG_XMM14: 
            case REG_XMM15: ICE_ASSERT(((void)"xmm registers have no 4-byte equivalent", false));
        }
    }
}

//
// Emit an immediate value.
//
static void emit_imm(EmitState *state, unsigned long val, OperandSize os)
{
    switch (os) {
        case OS_BYTE:   val &= 0xff; break;
        case OS_DWORD:  val &= 0xffffffff; break;
        case OS_QWORD:  break;
    }


    fprintf(state->out, "$%lu", val);
}

//
// Emit a memory reference.
//
static void emit_memory(EmitState *state, AsmMemoryOperand *mem)
{
    fprintf(state->out, "%d(", mem->offset);
    emit_reg(state, mem->reg, OS_QWORD);
    fprintf(state->out, ")");
}

//
// Emit a data reference.
//
static void emit_data(EmitState *state, char *name)
{
    fprintf(state->out, "%s(%%rip)", name);
}

//
// Emit an assembly operand.
//
static void emit_asmoper(EmitState *state, AsmOperand *oper, OperandSize os)
{
    switch (oper->tag) {
        case AOP_IMM:   emit_imm(state, oper->imm, os); break;
        case AOP_REG:   emit_reg(state, oper->reg, os); break;
        case AOP_MEMORY:emit_memory(state, &oper->memory); break;
        case AOP_DATA:  emit_data(state, oper->data); break;
        
        case AOP_PSEUDOREG:
            ICE_ASSERT(((void)"pseuedoreg operand found at code emission time.", false));
    }
}

//
// Emit a label.
// 
static void emit_label(EmitState *state, const char *label)
{
#ifndef __APPLE__
    fprintf(state->out, ".L%s", label);
#else
    fprintf(state->out, "L%s", label);
#endif

}

//
// Emit a return instruction.
//
static void emit_ret(EmitState *state)
{
    fprintf(state->out, "        movq\t%%rbp, %%rsp\n");
    fprintf(state->out, "        popq\t%%rbp\n");
    fprintf(state->out, "        ret\n");
}

//
// Emit a CDQ instruction.
//
static void emit_cdq(EmitState *state, AsmCdq *cdq)
{
    OperandSpec spec;
    type_to_operand_spec(cdq->type, &spec);

    switch (spec.size) {
        case OS_DWORD:  fprintf(state->out, "        cdq\n"); break;
        case OS_QWORD:  fprintf(state->out, "        cqo\n"); break;
        case OS_BYTE:   ICE_ASSERT(((void)"invalid size for emit_cdq", false));
    }
}

//
// Emit a mov instruction.
//
static void emit_mov(EmitState *state, AsmMov *mov)
{
    OperandSpec spec;
    type_to_operand_spec(mov->type, &spec);
    
    fprintf(state->out, "        mov%s\t", spec.suffix);
    emit_asmoper(state, mov->src, spec.size);
    fprintf(state->out, ", ");
    emit_asmoper(state, mov->dst, spec.size);
    fprintf(state->out, "\n");
}

//
// Emit a movsx instruction.
//
static void emit_movsx(EmitState *state, AsmMovsx *movsx)
{
    fprintf(state->out, "        movslq\t");
    emit_asmoper(state, movsx->src, OS_DWORD);
    fprintf(state->out, ", ");
    emit_asmoper(state, movsx->dst, OS_QWORD);
    fprintf(state->out, "\n");
}

//
// Emit an lea instruction.
//
static void emit_lea(EmitState *state, AsmLea *lea)
{
    fprintf(state->out, "        leaq\t");
    emit_asmoper(state, lea->src, OS_QWORD);
    fprintf(state->out, ", ");
    emit_asmoper(state, lea->dst, OS_QWORD);
    fprintf(state->out, "\n");
}

//
// Emit a unary instruction.
//
static void emit_unary(EmitState *state, AsmUnary *unary)
{
    char *opcode = "???";

    OperandSpec spec;
    type_to_operand_spec(unary->type, &spec);

    switch (unary->op) {
        case UOP_PLUS:          return;
        case UOP_MINUS:         opcode = "neg"; break;
        case UOP_COMPLEMENT:    opcode = "not"; break;
        case UOP_SHR:           opcode = "shr"; break;

        default:
            ICE_ASSERT(((void)"invalid unary opcode in emit_unary", false));
    }

    fprintf(state->out, "        %s%s\t", opcode, spec.suffix);
    emit_asmoper(state, unary->arg, spec.size);
    fprintf(state->out, "\n");
}

//
// Emit a binary instruction.
//
static void emit_binary(EmitState *state, AsmBinary *binary)
{
    char *opcode = "???";

    OperandSpec spec;
    type_to_operand_spec(binary->type, &spec);

    switch (binary->op) {
        case BOP_ADD:           opcode = "add"; break;
        case BOP_SUBTRACT:      opcode = "sub"; break;
        case BOP_MULTIPLY:      opcode = "imul"; break;
        case BOP_LSHIFT:        opcode = "shl"; break;
        case BOP_RSHIFT:        opcode = "sar"; break;
        case BOP_BITAND:        opcode = "and"; break;
        case BOP_BITOR:         opcode = "or"; break;
        case BOP_BITXOR:        opcode = "xor"; break;
        case BOP_DIVDBL:        opcode = "div"; break;

        //
        // NOTE idiv is handled as a special case.
        //
        default:
            ICE_ASSERT(((void)"invalid binary opcode in emit_binary", false));
    }

    if (binary->type->tag == AT_DOUBLE && binary->op == BOP_BITXOR) {
        fprintf(state->out, "        xorpd\t");
    } else if (binary->type->tag == AT_DOUBLE && binary->op == BOP_MULTIPLY) {
        fprintf(state->out, "        mulsd\t");
    } else {
        fprintf(state->out, "        %s%s\t", opcode, spec.suffix);
    }

    if (binary->op == BOP_RSHIFT || binary->op == BOP_LSHIFT) {
        emit_asmoper(state, binary->src, OS_BYTE);
    } else {
        emit_asmoper(state, binary->src, spec.size);
    }

    fprintf(state->out, ", ");
    emit_asmoper(state, binary->dst, spec.size);
    fprintf(state->out, "\n");
}

//
// Emit an IDIV instruction.
//
static void emit_idiv(EmitState *state, AsmIdiv *idiv)
{
    OperandSpec spec;
    type_to_operand_spec(idiv->type, &spec);

    fprintf(state->out, "        idiv%s\t", spec.suffix);
    emit_asmoper(state, idiv->arg, spec.size);
    fprintf(state->out, "\n");
}

//
// Emit a DIV instruction.
//
static void emit_div(EmitState *state, AsmDiv *div)
{
    OperandSpec spec;
    type_to_operand_spec(div->type, &spec);

    fprintf(state->out, "        div%s\t", spec.suffix);
    emit_asmoper(state, div->arg, spec.size);
    fprintf(state->out, "\n");
}

//
// Emit a CMP instruction.
//
static void emit_cmp(EmitState *state, AsmCmp *cmp)
{
    OperandSpec spec;
    type_to_operand_spec(cmp->type, &spec);

    if (cmp->type->tag == AT_DOUBLE) {
        fprintf(state->out, "        comisd\t");
    } else {
        fprintf(state->out, "        cmp%s\t", spec.suffix);
    }
    emit_asmoper(state, cmp->left, spec.size);
    fprintf(state->out, ", ");
    emit_asmoper(state, cmp->right, spec.size);
    fprintf(state->out, "\n");
}

//
// Emit a JMP instruction.
//
static void emit_jump(EmitState *state, AsmJump *jump)
{
    fprintf(state->out, "        jmp\t");
    emit_label(state, jump->target);
    fprintf(state->out, "\n");
}

//
// Emit a JMPCC instruction.
//
static void emit_jumpcc(EmitState *state, AsmJumpCc *jumpcc)
{
    fprintf(state->out, "        j%s\t", acc_describe(jumpcc->cc));
    emit_label(state, jumpcc->target);
    fprintf(state->out, "\n");
}

//
// Emit a SETCC instruction.
//
static void emit_setcc(EmitState *state, AsmSetCc *setcc)
{
    fprintf(state->out, "        set%s\t", acc_describe(setcc->cc));
    emit_asmoper(state, setcc->dst, OS_BYTE);
    fprintf(state->out, "\n");
}

//
// Emit a label.
//
static void emit_label_instr(EmitState *state, AsmLabel *label)
{
    emit_label(state, label->label);
    fprintf(state->out, ":\n");
}

//
// Emit code to reserve locals on the stack.
//
static void emit_stack_reserve(EmitState *state, AsmStackReserve *reserve)
{
    fprintf(state->out, "        subq\t$%d, %%rsp\n", reserve->bytes);
}

//
// Emit code to free locals on the stack.
//
static void emit_stack_free(EmitState *state, AsmStackFree *reserve)
{
    fprintf(state->out, "        addq\t$%d, %%rsp\n", reserve->bytes);
}

//
// Emit a push.
//
static void emit_push(EmitState *state, AsmPush *push)
{
    fprintf(state->out, "        push\t");
    emit_asmoper(state, push->value, OS_QWORD);
    fprintf(state->out, "\n");
}

//
// Emit a call.
//
static void emit_call(EmitState *state, AsmCall *call)
{
#ifdef __APPLE__
    fprintf(state->out, "        call\t_%s\n", call->id);
#else
    fprintf(state->out, "        call\t%s\n", call->id);
#endif
}

//
// Emit a function.
//
static void emit_function(EmitState *state, AsmFunction *func, FileLine *loc)
{
#ifdef __APPLE__
    if (func->global) {
        fprintf(state->out, "        .globl\t_%s\n", func->name);
    }
    fprintf(state->out, "        .text\n");
    fprintf(state->out, "_%s:\n", func->name);
#else
    if (func->global) {   
        fprintf(state->out, "        .globl\t%s\n", func->name);
    }
    fprintf(state->out, "        .text\n");
    fprintf(state->out, "%s:\n", func->name);
#endif
    fprintf(state->out, "        pushq\t%%rbp\n");
    fprintf(state->out, "        movq\t%%rsp, %%rbp\n");

    for (ListNode *curr = func->body.head; curr; curr = curr->next) {
        AsmNode *node = CONTAINER_OF(curr, AsmNode, list);
        emitcode_recurse(state, node, loc);
    }    
}

//
// Emit a static variable.
//
void emit_static_var(EmitState *state, AsmStaticVar *var)
{
    BackEndSymbol *sym = bstab_lookup(state->bstab, var->name);
    ICE_ASSERT(sym->tag == BST_OBJECT);
    int size = asmtype_size(sym->object.type);

    bool is_quad = size == 8;

    if (var->global) {
        fprintf(state->out, "        .globl\t%s\n", var->name);        
    }

    if (sym->object.type->tag == AT_DOUBLE) {
        fprintf(state->out, "        .data\n");
        fprintf(state->out, "        .balign\t%d\n", var->alignment);
        fprintf(state->out, "%s:\n", var->name);
        fprintf(state->out, "        .double\t%.20g\n", var->init.floatval); 
    } else if (var->init.intval.value == 0) {
        fprintf(state->out, "        .bss\n");
        fprintf(state->out, "        .balign\t%d\n", var->alignment);
        fprintf(state->out, "%s:\n", var->name);
        fprintf(state->out, "        .zero\t%d\n", size);
    } else {
        fprintf(state->out, "        .data\n");
        fprintf(state->out, "        .balign\t%d\n", var->alignment);
        fprintf(state->out, "%s:\n", var->name);
        fprintf(state->out, "        .%s\t%lu\n", 
            is_quad ? "quad" : "long",
            is_quad ? var->init.intval.value : (var->init.intval.value & 0xffffffff));
    }
    fprintf(state->out, "        .text\n");
}

//
// Emit a static constant.
//
void emit_static_const(EmitState *state, AsmStaticConst *cn)
{
    BackEndSymbol *sym = bstab_lookup(state->bstab, cn->name);
    ICE_ASSERT(sym->tag == BST_OBJECT);
    int size = asmtype_size(sym->object.type);

    bool is_quad = size == 8;

    fprintf(state->out, "        .section\t.rodata\n");
    if (sym->object.type->tag == AT_DOUBLE) {
        fprintf(state->out, "        .balign\t%d\n", cn->alignment);
        fprintf(state->out, "%s:\n", cn->name);
        fprintf(state->out, "        .double\t%.20g\n", cn->init.floatval); 
    } else if (cn->init.intval.value == 0) {
        fprintf(state->out, "        .balign\t%d\n", cn->alignment);
        fprintf(state->out, "%s:\n", cn->name);
        fprintf(state->out, "        .zero\t%d\n", size);
    } else {
        fprintf(state->out, "        .balign\t%d\n", cn->alignment);
        fprintf(state->out, "%s:\n", cn->name);
        fprintf(state->out, "        .%s\t%lu\n", 
            is_quad ? "quad" : "long",
            is_quad ? cn->init.intval.value : (cn->init.intval.value & 0xffffffff));
    }
    fprintf(state->out, "        .text\n");
}

//
// Emit a cvttsd2si instruction.
//
void emit_cvttsd2si(EmitState *state, AsmCvttsd2si *cvttsd2si)
{
    OperandSpec spec;
    type_to_operand_spec(cvttsd2si->type, &spec);

    fprintf(state->out, "        cvttsd2si%s\t", spec.suffix);
    emit_asmoper(state, cvttsd2si->src, OS_QWORD);
    fprintf(state->out, ", ");
    emit_asmoper(state, cvttsd2si->dst, spec.size);
    fprintf(state->out, "\n");
}

//
// Emit a cvtsi2sd instruction.
//
void emit_cvtsi2sd(EmitState *state, AsmCvtsi2sd *cvtsi2sd)
{
    OperandSpec spec;
    type_to_operand_spec(cvtsi2sd->type, &spec);

    fprintf(state->out, "        cvtsi2sd%s\t", spec.suffix);
    emit_asmoper(state, cvtsi2sd->src, spec.size);
    fprintf(state->out, ", ");
    emit_asmoper(state, cvtsi2sd->dst, OS_QWORD);
    fprintf(state->out, "\n");
}

//
// emit a top-level program.
// 
static void emit_program(EmitState *state, AsmProgram *prog)
{
    for (ListNode *curr = prog->funcs.head; curr; curr = curr->next) {
        AsmNode *func = CONTAINER_OF(curr, AsmNode, list);
        emitcode_recurse(state, func, &func->loc);        
    }

#ifndef __APPLE__
    fprintf(state->out, "        .section .note.GNU-stack,\"\",@progbits\n");
#endif
}

//
// Write actual assembly code out.
//
static void emitcode_recurse(EmitState *state, AsmNode *node, FileLine *loc)
{
    if (node->loc.fname != loc->fname || node->loc.line != loc->line) {
        *loc = node->loc;
        char *sloc = fileline_describe(loc);
#ifndef __APPLE__
        // 
        // TODO figure out why Mac assembler doesn't like this
        // line number format.
        //
        fprintf(state->out, "# %s\n", sloc);
#endif
        safe_free(sloc);
    }

    switch (node->tag) {
        case ASM_PROG:          emit_program(state, &node->prog); break;
        case ASM_FUNC:          emit_function(state, &node->func, loc); break;
        case ASM_STATIC_VAR:    emit_static_var(state, &node->static_var); break;
        case ASM_STATIC_CONST:  emit_static_const(state, &node->static_const); break;
        case ASM_STACK_RESERVE: emit_stack_reserve(state, &node->stack_reserve); break;
        case ASM_STACK_FREE:    emit_stack_free(state, &node->stack_free); break;
        case ASM_MOV:           emit_mov(state, &node->mov); break;
        case ASM_MOVSX:         emit_movsx(state, &node->movsx); break;
        case ASM_MOVZX:         ICE_ASSERT(((void)"untranslated movzx in emitcode", false));
        case ASM_LEA:           emit_lea(state, &node->lea); break;
        case ASM_UNARY:         emit_unary(state, &node->unary); break;
        case ASM_BINARY:        emit_binary(state, &node->binary); break;
        case ASM_CMP:           emit_cmp(state, &node->cmp); break;
        case ASM_JUMP:          emit_jump(state, &node->jump); break;
        case ASM_JUMPCC:        emit_jumpcc(state, &node->jumpcc); break;
        case ASM_SETCC:         emit_setcc(state, &node->setcc); break;
        case ASM_LABEL:         emit_label_instr(state, &node->label); break;
        case ASM_RET:           emit_ret(state); break;
        case ASM_CDQ:           emit_cdq(state, &node->cdq); break;
        case ASM_IDIV:          emit_idiv(state, &node->idiv); break;
        case ASM_DIV:           emit_div(state, &node->div); break;
        case ASM_PUSH:          emit_push(state, &node->push); break;
        case ASM_CALL:          emit_call(state, &node->call); break;
        case ASM_CVTTSD2SI:     emit_cvttsd2si(state, &node->cvttsd2si); break;
        case ASM_CVTSI2SD:      emit_cvtsi2sd(state, &node->cvtsi2sd); break;
    }
}

//
// Top level entry to emit code.
//
void emitcode(FILE *out, AsmNode *prog, BackEndSymbolTable *bstab)
{
    FileLine loc = { NULL, 0 };
    EmitState state = { out, bstab };

    emitcode_recurse(&state, prog, &loc);
}
