#pragma once

#include "constant.h"
#include "fileline.h"
#include "list.h"
#include "operators.h"
#include "symtab.h"

#include <stdbool.h>

typedef enum {
    REG_RAX,
    REG_RCX,
    REG_RDX,
    REG_RDI,
    REG_RSI,
    REG_R8,
    REG_R9,
    REG_R10,
    REG_R11,
    REG_RSP,
    REG_RBP,

    REG_XMM0,
    REG_XMM1,
    REG_XMM2,
    REG_XMM3,
    REG_XMM4,
    REG_XMM5,
    REG_XMM6,
    REG_XMM7,
    REG_XMM8,
    REG_XMM9,
    REG_XMM10,
    REG_XMM11,
    REG_XMM12,
    REG_XMM13,
    REG_XMM14,
    REG_XMM15,
} Register;

extern char *reg_name(Register reg);
extern bool is_xmm(Register reg);

typedef enum {
    AT_LONGWORD,
    AT_QUADWORD,
    AT_DOUBLE,
} AsmTypeTag;

typedef struct {
    AsmTypeTag tag;
} AsmType;

typedef enum  {
    AOP_IMM,            // an immediate (constant) value
    AOP_REG,            // the contents of a register
    AOP_PSEUDOREG,      // a named variable used like a register
    AOP_DATA,           // a variable reference to a static object
    AOP_MEMORY,         // reg + offset addressing, for RBP + xxx
} AsmOperandTag;

extern AsmType *asmtype_long(void);
extern AsmType *asmtype_quad(void);
extern AsmType *asmtype_double(void);
extern AsmType *asmtype_clone(AsmType *type);
extern void asmtype_free(AsmType *type);
extern char *asmtype_describe(AsmType *type);
extern int asmtype_size(AsmType *type);
extern int asmtype_alignment(AsmType *type);


typedef enum {
    ACC_E,
    ACC_NE,
    ACC_G,
    ACC_GE,
    ACC_L,
    ACC_LE,
    ACC_A,
    ACC_AE,
    ACC_B,
    ACC_BE,
} AsmConditionCode;

extern const char *acc_describe(AsmConditionCode cc);

typedef struct {
    Register reg;           // base register
    int offset;             // offset 
} AsmMemoryOperand;

typedef struct {
    AsmOperandTag tag;

    union {
        Register            reg;        // AOP_REG
        unsigned long       imm;        // AOP_IMM
        char               *pseudoreg;  // AOP_PSEUDOREG 
        char               *data;       // AOP_DATA
        AsmMemoryOperand    memory;     // AOP_MEMORY
    };
} AsmOperand;

extern AsmOperand *aoper_clone(AsmOperand *oper);
extern AsmOperand *aoper_reg(Register reg);
extern AsmOperand *aoper_pseudoreg(char *name);
extern AsmOperand *aoper_imm(unsigned long val);
extern AsmOperand *aoper_data(char *name);
extern AsmOperand *aoper_memory(Register reg, int offset);
extern bool aoper_is_mem(AsmOperand *oper);
extern void aoper_free(AsmOperand *op);
extern void aoper_print(AsmOperand *op);

typedef enum {
    ASM_PROG,
    ASM_FUNC,
    ASM_STATIC_VAR,
    ASM_STATIC_CONST,
    ASM_MOV,
    ASM_MOVSX,
    ASM_MOVZX,
    ASM_LEA,
    ASM_UNARY,
    ASM_BINARY,
    ASM_CMP,
    ASM_IDIV,
    ASM_DIV,
    ASM_CDQ,
    ASM_JUMP,
    ASM_JUMPCC,
    ASM_LABEL,
    ASM_SETCC,
    ASM_RET,
    ASM_STACK_RESERVE,
    ASM_STACK_FREE,
    ASM_PUSH,
    ASM_CALL,
    ASM_CVTTSD2SI,
    ASM_CVTSI2SD,
} AsmNodeTag;

typedef struct AsmNode AsmNode;

typedef struct {
    List funcs;             // of <AsmNode>
} AsmProgram;

typedef struct {
    char *name;             // function name
    int locals_size;        // needed bytes for locals
    List body;              // list <AsmNode> of instructions
    bool global;            // function is globally visible
} AsmFunction;

typedef struct {
    char *name;             // variable name
    bool global;            // variable is globally visible
    int alignment;          // required alignment (power of two)
    Const init;             // initial value
} AsmStaticVar;

typedef struct {
    char *name;             // variable name
    int alignment;          // required alignment (power of two)
    Const init;             // initial value
} AsmStaticConst;

typedef struct {
    AsmOperand *src;        // source operand
    AsmOperand *dst;        // destination operand
    AsmType *type;          // operand size
} AsmMov;

typedef struct {
    AsmOperand *src;        // source operand
    AsmOperand *dst;        // destination operand
} AsmMovsx;

typedef struct {
    AsmOperand *src;        // source operand
    AsmOperand *dst;        // destination operand
} AsmMovzx;

typedef struct {
    AsmOperand *src;        // source operand
    AsmOperand *dst;        // destination operand
} AsmLea;

typedef struct {
    UnaryOp op;             // operator
    AsmOperand *arg;        // argument (src == dst)
    AsmType *type;          // operand size
} AsmUnary;

typedef struct {
    BinaryOp op;            // operator
    AsmOperand *src;        // source argument
    AsmOperand *dst;        // destination argument
    AsmType *type;          // operand size
} AsmBinary;

typedef struct {
    AsmOperand *left;       // left of comparison
    AsmOperand *right;      // right of comparison
    AsmType *type;          // operand size
} AsmCmp;

typedef struct {
    AsmOperand *arg;        // argument 
    AsmType *type;          // operand size
} AsmIdiv;

typedef struct {
    AsmOperand *arg;        // argument 
    AsmType *type;          // operand size
} AsmDiv;

typedef struct {
    AsmType *type;          // operand size
} AsmCdq;

typedef struct {
    char *target;           // label to jump to
} AsmJump;

typedef struct {
    char *target;           // label to jump to if cc is true
    AsmConditionCode cc;    // condition code to jump on
} AsmJumpCc;

typedef struct {
    char *label;            // name of the label
} AsmLabel;

typedef struct {
    AsmOperand *dst;        // value to set if cc is true
    AsmConditionCode cc;    // condition code to test
} AsmSetCc;

typedef struct {
    int bytes;              // number of bytes to reserve for local variables
} AsmStackReserve;

typedef struct {
    int bytes;              // number of bytes to pop off the stack
} AsmStackFree;

typedef struct {
    AsmOperand *value;      // value to push onto the stack
} AsmPush;

typedef struct {
    char *id;               // name of function to call
} AsmCall;

typedef struct {
    AsmOperand *src;        // source of conversion
    AsmOperand *dst;        // dest of conversion
    AsmType *type;          // operand size
} AsmCvttsd2si;

typedef struct {
    AsmOperand *src;        // source of conversion
    AsmOperand *dst;        // dest of conversion
    AsmType *type;          // operand size
} AsmCvtsi2sd;

struct AsmNode {
    AsmNodeTag tag;         // discriminated union tag
    ListNode list;          // keep instructions in list
    FileLine loc;           // location in original source

    union {
        AsmProgram          prog;               // ASM_PROG
        AsmFunction         func;               // ASM_FUNC
        AsmStaticVar        static_var;         // ASM_STATIC_VAR
        AsmStaticConst      static_const;       // ASM_STATIC_CONST
        AsmMov              mov;                // ASM_MOV
        AsmMovsx            movsx;              // ASM_MOVSX
        AsmMovzx            movzx;              // ASM_MOVZX
        AsmUnary            unary;              // ASM_UNARY
        AsmLea              lea;                // ASM_LEA
        AsmBinary           binary;             // ASM_BINARY
        AsmCmp              cmp;                // ASM_CMP
        AsmIdiv             idiv;               // ASM_IDIV
        AsmDiv              div;                // ASM_DIV
        AsmCdq              cdq;                // ASM_CDQ
        AsmJump             jump;               // ASM_JUMP
        AsmJumpCc           jumpcc;             // ASM_JUMPCC
        AsmLabel            label;              // ASM_LABEL
        AsmSetCc            setcc;              // ASM_SETCC
        AsmStackReserve     stack_reserve;      // ASM_STACK_RESERVE
        AsmStackFree        stack_free;         // ASM_STACK_FREE
        AsmPush             push;               // ASM_PUSH
        AsmCall             call;               // ASM_CALL
        AsmCvttsd2si        cvttsd2si;          // ASM_CVTTSD2SI 
        AsmCvtsi2sd         cvtsi2sd;           // ASM_CVTSI2SD 
    };
};

extern AsmNode *asm_prog(List funcs, FileLine loc);
extern AsmNode *asm_func(char *name, List body, bool global, FileLine loc);
extern AsmNode *asm_static_var(char *name, bool global, int alignment, Const init, FileLine loc);
extern AsmNode *asm_static_const(char *name, int alignment, Const init, FileLine loc);
extern AsmNode *asm_mov(AsmOperand *src, AsmOperand *dst, AsmType *type, FileLine loc);
extern AsmNode *asm_movsx(AsmOperand *src, AsmOperand *dst, FileLine loc);
extern AsmNode *asm_movzx(AsmOperand *src, AsmOperand *dst, FileLine loc);
extern AsmNode *asm_lea(AsmOperand *src, AsmOperand *dst, FileLine loc);
extern AsmNode *asm_unary(UnaryOp op, AsmOperand *arg, AsmType *type, FileLine loc);
extern AsmNode *asm_binary(BinaryOp op, AsmOperand *src, AsmOperand *dst, AsmType *type, FileLine loc);
extern AsmNode *asm_cmp(AsmOperand *left, AsmOperand *right, AsmType *type, FileLine loc);
extern AsmNode *asm_idiv(AsmOperand *arg, AsmType *type, FileLine loc);
extern AsmNode *asm_div(AsmOperand *arg, AsmType *type, FileLine loc);
extern AsmNode *asm_cdq(AsmType *type, FileLine loc);
extern AsmNode *asm_jump(char *target, FileLine loc);
extern AsmNode *asm_jumpcc(char *target, AsmConditionCode cc, FileLine loc);
extern AsmNode *asm_label(char *label, FileLine loc);
extern AsmNode *asm_setcc(AsmOperand *dst, AsmConditionCode cc, FileLine loc);
extern AsmNode *asm_ret(FileLine loc);
extern AsmNode *asm_stack_reserve(int bytes, FileLine loc);
extern AsmNode *asm_stack_free(int bytes, FileLine loc);
extern AsmNode *asm_push(AsmOperand *value, FileLine loc);
extern AsmNode *asm_call(char *id, FileLine loc);
extern AsmNode *asm_cvttsd2si(AsmOperand *src, AsmOperand *dst, AsmType *type, FileLine loc);
extern AsmNode *asm_cvtsi2sd(AsmOperand *src, AsmOperand *dst, AsmType *type, FileLine loc);
extern void asm_free(AsmNode *node);
extern void asm_print(AsmNode *node, bool locs);

