#pragma once

#include "ast.h"
#include "constant.h"
#include "fileline.h"
#include "list.h"
#include "symtab.h"

//
// Three-address code - intermediate format.
//
typedef enum {
    TAC_PROGRAM,
    TAC_FUNCDEF,
    TAC_STATIC_VAR,
    TAC_RETURN,
    TAC_COPY,
    TAC_JUMP,
    TAC_JUMP_ZERO,
    TAC_JUMP_NOT_ZERO,
    TAC_LABEL,
    TAC_UNARY,
    TAC_BINARY,
    TAC_CONST,
    TAC_VAR,
    TAC_FUNCTION_CALL,
    TAC_SIGN_EXTEND,
    TAC_ZERO_EXTEND,
    TAC_TRUNCATE,
    TAC_DOUBLE_TO_INT,
    TAC_DOUBLE_TO_UINT,
    TAC_INT_TO_DOUBLE,
    TAC_UINT_TO_DOUBLE,
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
    bool global;                    // is the function globally visible
    List parms;                     // List<TacVar> of parameters
    List body;                      // list<TacNode> of instructions
} TacFuncDef;

//
// A static variable declaration.
//
typedef struct {
    char *name;                     // name
    bool global;                    // is the variable globally visible
    Type *type;                     // variable type
    Const init;                     // the initial value
} TacStaticVar;

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
// A variable (program defined or temporary).
//
typedef struct {
    char *name;                     // variable name
} TacVar;

//
// A function call.
//
typedef struct {
    char *name;                     // name of function to call
    List args;                      // list <TacNode> of arguments
    TacNode *dst;                   // where to save result
} TacFunctionCall;

//
// A sign extend operation.
//
typedef struct {
    TacNode *src;
    TacNode *dst;
} TacSignExtend;

//
// A zero extend operation.
//
typedef struct {
    TacNode *src;
    TacNode *dst;
} TacZeroExtend;

//
// A truncate operation.
//
typedef struct {
    TacNode *src;
    TacNode *dst;
} TacTruncate;

//
// A double to int operation.
//
typedef struct {
    TacNode *src;
    TacNode *dst;
} TacDoubleToInt;

//
// A double to unsigned int operation.
//
typedef struct {
    TacNode *src;
    TacNode *dst;
} TacDoubleToUInt;

//
// An int to double operation.
//
typedef struct {
    TacNode *src;
    TacNode *dst;
} TacIntToDouble;

//
// An unsigned int to double operation.
//
typedef struct {
    TacNode *src;
    TacNode *dst;
} TacUIntToDouble;

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
        TacStaticVar    static_var;
        TacReturn       ret;
        TacCopy         copy;
        TacJump         jump;
        TacJumpZero     jump_zero;
        TacJumpNotZero  jump_not_zero;
        TacLabel        label;
        TacUnary        unary;
        TacBinary       binary;
        Const           constant;
        TacVar          var;
        TacFunctionCall call;
        TacSignExtend   sign_extend;
        TacZeroExtend   zero_extend;
        TacTruncate     truncate;
        TacDoubleToInt  dbl_to_int;
        TacDoubleToUInt dbl_to_uint;
        TacIntToDouble  int_to_dbl;
        TacUIntToDouble uint_to_dbl;
    };
} TacNode;

extern TacNode *tac_program(List decls, FileLine loc);
extern TacNode *tac_function_def(char *name, bool global, List parms, List body, FileLine loc);
extern TacNode *tac_static_var(char *name, bool global, Type *type, Const init, FileLine loc);
extern TacNode *tac_return(TacNode *val, FileLine loc);
extern TacNode *tac_copy(TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_jump(char *target, FileLine loc);
extern TacNode *tac_jump_zero(TacNode *condition, char *target, FileLine loc);
extern TacNode *tac_jump_not_zero(TacNode *condition, char *target, FileLine loc);
extern TacNode *tac_label(char *name, FileLine loc);
extern TacNode *tac_unary(UnaryOp op, TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_binary(BinaryOp op, TacNode *left, TacNode *right, TacNode *dst, FileLine loc);
extern TacNode *tac_const(Const cn, FileLine loc);
extern TacNode *tac_var(char *name, FileLine loc);
extern TacNode *tac_function_call(char *name, List args, TacNode *dst, FileLine loc);
extern TacNode *tac_sign_extend(TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_zero_extend(TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_truncate(TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_double_to_int(TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_double_to_uint(TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_int_to_double(TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_uint_to_double(TacNode *src, TacNode *dst, FileLine loc);

extern TacNode *tac_clone_operand(TacNode *tac);

extern void tac_free(TacNode *tac);
extern void tac_print(TacNode *tac, bool locs);

