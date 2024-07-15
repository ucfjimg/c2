#pragma once

#include "fileline.h"
#include "list.h"

#include <stdbool.h>

typedef enum {
    REG_RAX,
} Register;

extern char *reg_name(Register reg);

typedef enum  {
    AOP_IMM,
    AOP_REG,
} AsmOperandTag;

typedef struct {
    AsmOperandTag tag;

    union {
        Register reg;       // AOP_REG
        unsigned long imm;  // AOP_IMM 
    };
} AsmOperand;

extern AsmOperand *aoper_reg(Register reg);
extern AsmOperand *aoper_imm(unsigned long val);
extern void aoper_free(AsmOperand *op);
extern void aoper_print(AsmOperand *op);

typedef enum {
    ASM_PROG,
    ASM_FUNC,
    ASM_MOV,
    ASM_RET,
} AsmNodeTag;

typedef struct AsmNode AsmNode;

typedef struct {
    AsmNode *func;          // the function
} AsmProgram;

typedef struct {
    char *name;             // function name
    List body;              // list <AsmNode> of instructions
} AsmFunction;

typedef struct {
    AsmOperand *src;        // source operand
    AsmOperand *dst;        // destination operand
} AsmMov;

struct AsmNode {
    AsmNodeTag tag;         // discriminated union tag
    ListNode list;          // keep instructions in list
    FileLine loc;           // location in original source

    union {
        AsmProgram prog;    // ASM_PROG
        AsmFunction func;   // ASM_FUNC
        AsmMov mov;         // ASM_MOV
    };
};

extern AsmNode *asm_prog(void);
extern AsmNode *asm_func(char *name);
extern AsmNode *asm_mov(AsmOperand *src, AsmOperand *dst);
extern AsmNode *asm_ret(void);
extern void asm_free(AsmNode *node);
extern void asm_print(AsmNode *node, bool locs);

