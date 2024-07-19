#pragma once

#include "fileline.h"
#include "list.h"
#include "operators.h"

#include <stdbool.h>

typedef enum {
    REG_RAX,
    REG_R10,
} Register;

extern char *reg_name(Register reg);

typedef enum  {
    AOP_IMM,
    AOP_REG,
    AOP_PSEUDOREG,
    AOP_STACK,
} AsmOperandTag;

typedef struct {
    AsmOperandTag tag;

    union {
        Register reg;       // AOP_REG
        unsigned long imm;  // AOP_IMM
        char *pseudoreg;    // AOP_PSEUDOREG 
        int stack_offset;   // AOP_STACK, offset from frame pointer
    };
} AsmOperand;

extern AsmOperand *aoper_clone(AsmOperand *oper);
extern AsmOperand *aoper_reg(Register reg);
extern AsmOperand *aoper_pseudoreg(char *name);
extern AsmOperand *aoper_imm(unsigned long val);
extern AsmOperand *aoper_stack(int val);
extern void aoper_free(AsmOperand *op);
extern void aoper_print(AsmOperand *op);

typedef enum {
    ASM_PROG,
    ASM_FUNC,
    ASM_MOV,
    ASM_UNARY,
    ASM_RET,
    ASM_STACK_RESERVE,
} AsmNodeTag;

typedef struct AsmNode AsmNode;

typedef struct {
    AsmNode *func;          // the function
} AsmProgram;

typedef struct {
    char *name;             // function name
    int locals_size;        // needed bytes for locals
    List body;              // list <AsmNode> of instructions
} AsmFunction;

typedef struct {
    AsmOperand *src;        // source operand
    AsmOperand *dst;        // destination operand
} AsmMov;

typedef struct {
    UnaryOp op;             // operator
    AsmOperand *arg;        // argument (src == dst)
} AsmUnary;

typedef struct {
    int bytes;              // number of bytes to reserve for local variables
} AsmStackReserve;

struct AsmNode {
    AsmNodeTag tag;         // discriminated union tag
    ListNode list;          // keep instructions in list
    FileLine loc;           // location in original source

    union {
        AsmProgram prog;                // ASM_PROG
        AsmFunction func;               // ASM_FUNC
        AsmMov mov;                     // ASM_MOV
        AsmUnary unary;                 // ASM_UNARY
        AsmStackReserve stack_reserve;  // ASM_STACK_RESERVE
    };
};

extern AsmNode *asm_prog(void);
extern AsmNode *asm_func(char *name, List body);
extern AsmNode *asm_mov(AsmOperand *src, AsmOperand *dst);
extern AsmNode *asm_unary(UnaryOp up, AsmOperand *arg);
extern AsmNode *asm_ret(void);
extern AsmNode *asm_stack_reserve(int bytes);
extern void asm_free(AsmNode *node);
extern void asm_print(AsmNode *node, bool locs);

