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
    TAC_UNARY,
    TAC_CONST_INT,
    TAC_VAR,
} TacTag;

typedef struct TacNode TacNode;

typedef struct {
    TacNode *func;                  // single function
} TacProgram;

typedef struct {
    char *name;                     // name
    List body;                      // list<TacNode*> of instructions
} TacFuncDef;

typedef struct {
    TacNode *val;                   // value to return (can be NULL)
} TacReturn;

typedef struct {
    UnaryOp op;                     // operation
    TacNode *src;                   // operand to operate on
    TacNode *dst;                   // where to store result
} TacUnary;

typedef struct {
    unsigned long val;              // constant value
} TacConstInt;

typedef struct {
    char *name;                     // variable name
} TacVar;

typedef struct TacNode {
    ListNode list;

    TacTag tag;
    FileLine loc;

    union {
        TacProgram  prog;
        TacFuncDef  funcdef;
        TacReturn   ret;
        TacUnary    unary;
        TacConstInt constint;
        TacVar      var;
    };
} TacNode;

extern TacNode *tac_program(TacNode *func, FileLine loc);
extern TacNode *tac_function_def(char *name, List body, FileLine loc);
extern TacNode *tac_return(TacNode *val, FileLine loc);
extern TacNode *tac_unary(UnaryOp op, TacNode *src, TacNode *dst, FileLine loc);
extern TacNode *tac_const_int(unsigned long val, FileLine loc);
extern TacNode *tac_var(char *name, FileLine loc);

extern void tac_free(TacNode *tac);
extern void tac_print(TacNode *tac, bool locs);

