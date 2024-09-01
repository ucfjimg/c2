#include "emitcode.h"

#include "fileline.h"
#include "ice.h"
#include "safemem.h"
#include "strbuilder.h"

#include <ctype.h>

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
        case AT_BYTE:
            spec->suffix = "b";
            spec->size = OS_BYTE;
            break;

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

        case AT_BYTEARRAY:
            spec->suffix = "<byte-array>";
            spec->size = OS_QWORD;
            break;

        default:
            spec->suffix = "<invalid-tag>";
            spec->size = OS_QWORD;
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
            case REG_XMM15: ICE_ASSERT(((void)"xmm registers have no byte equivalent", false));
        }
    }
}

//
// Convert a string literal to an allocated assembler-friendly string.
//
// If there is a nul terminator, don't emit it and use .asciz instead.
//
static char *strlit_to_asm(char *data, size_t len, bool nul_terminated)
{
    StrBuilder *stb = stb_alloc();

    if (nul_terminated && len && data[len-1] == 0) {
        len--;
    }

    for (size_t i = 0; i < len; i++) {
        int ch = (unsigned char)data[i];
        
        if (ch == '\\' || ch == '"') {
            stb_push_char(stb, '\\');
        }
        
        if (isprint(ch)) {
            stb_push_char(stb, ch);
        } else {
            stb_printf(stb, "\\%03o", ch); 
        }
    }

    char *strlit = stb_take(stb);
    stb_free(stb);
    return strlit;
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


    fprintf(state->out, "$0x%016lx", val);
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
static void emit_data(EmitState *state, AsmDataOperand *data)
{
    ICE_NYI("emit_data::data");
//    fprintf(state->out, "%s(%%rip)", name);
}

//
// Emit a scale-index-base operand.
//
static void emit_indexed(EmitState *state, AsmIndexedOperand *index)
{
    fprintf(state->out, "(");
    emit_reg(state, index->base, OS_QWORD);
    fprintf(state->out, ", ");
    emit_reg(state, index->index, OS_QWORD);
    fprintf(state->out, ", %d)", index->scale);
}

//
// Emit an assembly operand.
//
static void emit_asmoper(EmitState *state, AsmOperand *oper, OperandSize os)
{
    switch (oper->tag) {
        case AOP_IMM:       emit_imm(state, oper->imm, os); break;
        case AOP_REG:       emit_reg(state, oper->reg, os); break;
        case AOP_MEMORY:    emit_memory(state, &oper->memory); break;
        case AOP_DATA:      emit_data(state, &oper->data); break;
        case AOP_INDEXED:   emit_indexed(state, &oper->indexed); break;

        case AOP_PSEUDOMEM:
            ICE_ASSERT(((void)"pseuedomem operand found at code emission time.", false));
        
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
    OperandSpec src;
    OperandSpec dst;

    type_to_operand_spec(movsx->src_type, &src);
    type_to_operand_spec(movsx->dst_type, &dst);    

    fprintf(state->out, "        movs%s%s\t",
        src.suffix,
        dst.suffix);

    emit_asmoper(state, movsx->src, src.size);
    fprintf(state->out, ", ");
    emit_asmoper(state, movsx->dst, dst.size);
    fprintf(state->out, "\n");
}

//
// Emit a movzx instruction.
//
static void emit_movzx(EmitState *state, AsmMovzx *movzx)
{
    OperandSpec src;
    OperandSpec dst;

    type_to_operand_spec(movzx->src_type, &src);
    type_to_operand_spec(movzx->dst_type, &dst);    

    fprintf(state->out, "        movz%s%s\t",
        src.suffix,
        dst.suffix);

    emit_asmoper(state, movzx->src, OS_DWORD);
    fprintf(state->out, ", ");
    emit_asmoper(state, movzx->dst, OS_QWORD);
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
        case BOP_URSHIFT:       opcode = "shr"; break;
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
    if (var->global) {
        fprintf(state->out, "        .globl\t%s\n", var->name);        
    }

    if (var->init.head && var->init.head->next == NULL) {
        StaticInitializer *si = CONTAINER_OF(var->init.head, StaticInitializer, list);

        if (si->tag == SI_ZERO || (si->cval.tag == CON_INTEGRAL && si->cval.intval.value == 0)) {
            fprintf(state->out, "        .bss\n");
            fprintf(state->out, "        .balign\t%d\n", var->alignment);
            fprintf(state->out, "%s:\n", var->name);

            if (si->tag == SI_ZERO) {
                fprintf(state->out, "        .zero\t%zd\n", si->bytes);
            } else if (si->cval.intval.size == CIS_INT) {
                fprintf(state->out, "        .long\t0\n");
            } else {
                fprintf(state->out, "        .quad\t0\n");
            }

            fprintf(state->out, "        .text\n");
            return;
        }
    }

    fprintf(state->out, "        .data\n");
    fprintf(state->out, "        .balign\t%d\n", var->alignment);
    fprintf(state->out, "%s:\n", var->name);

    //
    // NB the codegen pass is responsible for ensuring that the size of the
    // constants in the initializer list is correct for the variable size.
    //
    for (ListNode *curr = var->init.head; curr; curr = curr->next) {
        StaticInitializer *si = CONTAINER_OF(curr, StaticInitializer, list);

        switch (si->tag) {
            case SI_ZERO:
                fprintf(state->out, "        .zero\t%zd\n", si->bytes);
                break;

            case SI_CONST: {
                Const *pcn = &si->cval;

                if (pcn->tag == CON_FLOAT) {
                    fprintf(state->out, "        .double\t%.20g\n", pcn->floatval); 
                } else {
                    switch (pcn->intval.size) {
                        case CIS_CHAR:
                            fprintf(state->out, "        .byte\t0x%02x\n", (unsigned char)pcn->intval.value);
                            break;

                        case CIS_INT:
                            fprintf(state->out, "        .long\t0x%08x\n", (unsigned)pcn->intval.value);
                            break;

                        case CIS_LONG:
                            fprintf(state->out, "        .quad\t0x%016lx\n", pcn->intval.value);
                            break;
                    }
                } 
                break;
            }

            case SI_POINTER:
                fprintf(state->out, "        .quad\t%s\n", si->ptr_name);
                break;

            case SI_STRING: {
                char *strlit = strlit_to_asm(
                    si->string.data,
                    si->string.length,
                    si->string.nul_terminated
                );

                if (si->string.nul_terminated) {
                    fprintf(state->out, "        .asciz\t");
                } else {
                    fprintf(state->out, "        .ascii\t");
                }
                fprintf(state->out, "\"%s\"\n", strlit);
                safe_free(strlit);
                break;
            }
        }
    }

    fprintf(state->out, "        .text\n");
}

//
// Emit a static constant.
//
void emit_static_const(EmitState *state, AsmStaticConst *cn)
{
    fprintf(state->out, "        .section\t.rodata\n");
    fprintf(state->out, "        .balign\t%d\n", cn->alignment);
    fprintf(state->out, "%s:\n", cn->name);

    StaticInitializer *si = cn->init;
    char *strlit;

    switch (si->tag) {
        case SI_CONST:
            switch (si->cval.tag) {
                case CON_FLOAT:
                    fprintf(state->out, "        .double\t%.20g\n", si->cval.floatval);
                    break;

                case CON_INTEGRAL:
                    if (si->cval.intval.size == CIS_CHAR) {
                        fprintf(state->out, "        .byte\t0x%02lx\n", si->cval.intval.value & 0xff);
                    } else if (si->cval.intval.size == CIS_INT) {
                        fprintf(state->out, "        .long\t0x%08lx\n", si->cval.intval.value & 0xffffffff);
                    } else {
                        fprintf(state->out, "        .quad \t0x%016lx\n", si->cval.intval.value);
                    }
                    break;
            }
            break;

        case SI_POINTER:
            fprintf(state->out, "        .quad\t%s\n", si->ptr_name);
            break;

        case SI_STRING:
            strlit = strlit_to_asm(
                si->string.data,
                si->string.length,
                si->string.nul_terminated
            );

            if (si->string.nul_terminated) {
                fprintf(state->out, "        .asciz\t");
            } else {
                fprintf(state->out, "        .ascii\t");
            }
            fprintf(state->out, "\"%s\"\n", strlit);
            safe_free(strlit);
            break;

        case SI_ZERO:
            fprintf(state->out, "        .zero\t%zd\n", si->bytes);
            break;
    }





    #ifdef TODO
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

    #endif
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
        case ASM_MOVZX:         emit_movzx(state, &node->movzx); break;
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
