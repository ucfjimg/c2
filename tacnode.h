#pragma once

#include "ast.h"
#include "fileline.h"
#include "list.h"

//
// Three-address code - intermediate format.
//
typedef enum {
    TAC_PROGRAM,
    TAC_FUNCDEF,
    TAC_RETURN,
    TAC_COPY,
    TAC_JUMP,
    TAC_JUMP_ZERO,
    TAC_JUMP_NOT_ZERO,
    TAC_LABEL,
    TAC_UNARY,
    TAC_BINARY,
    TAC_CONST_INT,
    TAC_VAR,
} TacTag;

typedef struct TacNode TacNode;

//
// An entire program.
//
typedef struct {
    List decls;                     // Of <TacNode>
} TacProgram;

//
// A function definition.
//
typedef struct {
    char *name;                     // name
    List body;                      // list<TacNode*> of instructions
} TacFuncDef;

//
// A return statement with a value to return.
//
typedef struct {
    TacNode *val;                   // value to return (can be NULL)
} TacReturn;

//
// Copy a value from a source to a destination.
//
typedef struct {
    TacNode *src;                   // source value
    TacNode *dst;                   // destination
} TacCopy;

//
// Unconditional jump to a label.
//
typedef struct {
    char *target;                   // jump target
} TacJump;

//
// Jump to a label if an expression is zero.
//
typedef struct {
    TacNode *condition;             // condition to test, jump if zero
    char *target;                   // jump target
} TacJumpZero;

//
// Jump to a label if an expression is not zero.
//
typedef struct {
    TacNode *condition;             // condition to test, jump if not zero
    char *target;                   // jump target
} TacJumpNotZero;

//
// A label specifying a location in the program.
//
typedef struct {
    char *name;                     // the label's name
} TacLabel;

//
// A unary operator with a source expression and destination.
//
typedef struct {
    UnaryOp op;                     // operation
    TacNode *src;                   // operand to operate on
    TacNode *dst;                   // where to store result
} TacUnary;

//
// A binary operator with a left and right source expression and destination.
//
typedef struct {
    BinaryOp op;                    // operation
    TacNode *left;                  // left operand to operate on
    TacNode *right;                 // right operand to operate on
    TacNode *dst;                   // where to store result
} TacBinary;

//
// An integer constant.
//
typedef struct {
    unsigned long val;              // constant value
} TacConstInt;

//
// A variable (program defined or temporary).
//
typedef struct {
    char *name;                     // variable name
} TacVar;

//
// A TAC node -- discriminated union based on `tag`
//
typedef struct TacNode {
    ListNode list;
    TacTag tag;
    FileLine loc;

    union {
        TacProgram      prog;
        TacFuncDef      funcdef;
        TacReturn       ret;
        TacCopy         copy;
        TacJump         jump;
        TacJumpZero     jump_zero;
        TacJumpNotZero  jump_not_zero;
        TacLabel        label;
        TacUnary        unary;
        TacBinary       binary;
        TacConstInt     constint;
        TacVar          var;
    };
} TacNode;

extern TacNode *tac_program(List decls, FileLine loc);
extern TacNode *tac_function_def(char *name, List body, FileLine loc);
extern TacNode *tac_return(TacNode *val, FileLine loc);
extern TacNode *tac_copy(TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_jump(char *target, FileLine loc);
extern TacNode *tac_jump_zero(TacNode *condition, char *target, FileLine loc);
extern TacNode *tac_jump_not_zero(TacNode *condition, char *target, FileLine loc);
extern TacNode *tac_label(char *name, FileLine loc);
extern TacNode *tac_unary(UnaryOp op, TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_binary(BinaryOp op, TacNode *left, TacNode *right, TacNode *dst, FileLine loc);
extern TacNode *tac_const_int(unsigned long val, FileLine loc);
extern TacNode *tac_var(char *name, FileLine loc);

extern TacNode *tac_clone_operand(TacNode *tac);

extern void tac_free(TacNode *tac);
extern void tac_print(TacNode *tac, bool locs);

