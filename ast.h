#pragma once

#include "fileline.h"
#include "list.h"
#include "mempool.h"
#include "operators.h"
#include "type.h"

#include <stdbool.h>

typedef struct {
    MemPool *exp_pool;
    MemPool *decl_pool;
    MemPool *stmt_pool;
} AstState;

typedef struct Expression Expression;
typedef struct Declaration Declaration;
typedef struct Statement Statement;
typedef struct BlockItem BlockItem;

//
// expressions
//
typedef enum {
    EXP_SCHAR,
    EXP_UCHAR,
    EXP_INT,
    EXP_LONG,
    EXP_UINT,
    EXP_ULONG,
    EXP_FLOAT,
    EXP_STRING,
    EXP_VAR,
    EXP_UNARY,
    EXP_BINARY,
    EXP_CONDITIONAL,
    EXP_ASSIGNMENT,
    EXP_FUNCTION_CALL,
    EXP_CAST,
    EXP_DEREF,
    EXP_ADDROF,
    EXP_SUBSCRIPT,
    EXP_SIZEOF,
} ExpressionTag;

typedef struct {
    char *data;
    size_t length;
} ExpString;

typedef struct {
    char *name;
} ExpVar;

typedef struct {
    UnaryOp op;
    Expression *exp;
} ExpUnary;

typedef struct {
    BinaryOp op;
    Expression *left;
    Expression *right;
} ExpBinary;

typedef struct {
    Expression *cond;
    Expression *trueval;
    Expression *falseval;
} ExpConditional;

typedef struct {
    BinaryOp op;
    Expression *left;
    Expression *right;
} ExpAssignment;

typedef struct {
    char *name;
    List args;                              // of <Expression>
} ExpFunctionCall;

typedef struct {
    Type *type;
    Expression *exp;
} ExpCast;

typedef struct {
    Expression *exp;
} ExpDeref;

typedef struct {
    Expression *exp;
} ExpAddrOf;

typedef struct {
    Expression *left;
    Expression *right;
} ExpSubscript;

typedef enum {
    SIZEOF_TYPE,
    SIZEOF_EXP,
} SizeofTag;

typedef struct {
    SizeofTag tag;

    union {
        Type *type;
        Expression *exp;
    };
} ExpSizeof;

struct Expression {
    ExpressionTag tag;
    FileLine loc;       
    ListNode list;        
    Type *type;

    union {
        unsigned long intval;
        unsigned long longval;
        unsigned long uintval;
        unsigned long ulongval;
        double floatval;
        ExpString string;
        ExpVar var;
        ExpUnary unary;
        ExpBinary binary;
        ExpConditional conditional;
        ExpAssignment assignment;
        ExpFunctionCall call;
        ExpCast cast;
        ExpDeref deref;
        ExpAddrOf addrof;
        ExpSubscript subscript;
        ExpSizeof sizeof_;
    };
};

extern Expression *exp_schar(AstState *state, unsigned long intval, FileLine loc);
extern Expression *exp_uchar(AstState *state, unsigned long intval, FileLine loc);
extern Expression *exp_int(AstState *state, unsigned long intval, FileLine loc);
extern Expression *exp_long(AstState *state, unsigned long intval, FileLine loc);
extern Expression *exp_uint(AstState *state, unsigned long intval, FileLine loc);
extern Expression *exp_ulong(AstState *state, unsigned long intval, FileLine loc);
extern Expression *exp_float(AstState *state, double floatval, FileLine loc);
extern Expression *exp_string(AstState *state, char *data, size_t length, FileLine loc);
extern Expression *exp_var(AstState *state, char *name, FileLine loc);
extern Expression *exp_unary(AstState *state, UnaryOp op, Expression *exp, FileLine loc);
extern Expression *exp_binary(AstState *state, BinaryOp op, Expression *left, Expression *right, FileLine loc);
extern Expression *exp_conditional(AstState *state, Expression *conditional, Expression *trueval, Expression *falseval, FileLine loc);
extern Expression *exp_assignment(AstState *state, BinaryOp op, Expression *left, Expression *right, FileLine loc);
extern Expression *exp_function_call(AstState *state, char *name, List args, FileLine loc);
extern Expression *exp_cast(AstState *state, Type *type, Expression *exp, FileLine loc);
extern Expression *exp_deref(AstState *state, Expression *exp, FileLine loc);
extern Expression *exp_addrof(AstState *state, Expression *exp, FileLine loc);
extern Expression *exp_subscript(AstState *state, Expression *left, Expression *right, FileLine loc);
extern Expression *exp_sizeof_type(AstState *state, Type *type, FileLine loc);
extern Expression *exp_sizeof_exp(AstState *state, Expression *exp, FileLine loc);
extern void exp_set_type(Expression *exp, Type *type);
extern bool exp_is_constant(Expression *exp);
extern bool exp_is_int_constant(Expression *exp);


//
// initializers
//
typedef enum {
    INIT_SINGLE,
    INIT_COMPOUND,
} InitializerTag;

typedef struct {
    ListNode list;
    InitializerTag tag;
    Type *type;

    union {
        Expression  *single;        // INIT_SINGLE        
        List        compound;       // INIT_COMPOUND of <Initializer>
    };
} Initializer;

extern Initializer *init_single(Expression *exp);
extern Initializer *init_compound(List inits);

//
// declarations
//
typedef enum {
    DECL_VARIABLE,
    DECL_FUNCTION,
} DeclarationTag;

typedef struct {
    char *name;                 // variable name
    Initializer *init;          // optional initializer
    Type *type;                 // variable type
    StorageClass storage_class; // if the declaration is marked as static or extern
} DeclVariable;

typedef struct {
    ListNode list;
    char *name;                 // parameter name
} FuncParameter;

extern FuncParameter *func_parm(char *name);
extern void func_parm_free(FuncParameter *parm);

typedef struct {
    char *name;                 // function name
    List parms;                 // list <FuncParameter> of parameters
    List body;                  // if a definition, List <BlockItem>
    bool has_body;              // declaration is also a definition
    Type *type;                 // function type
    StorageClass storage_class; // if the declaration is marked as static or extern
} DeclFunction;

struct Declaration {
    ListNode list;
    DeclarationTag tag;
    FileLine loc;

    union {
        DeclVariable var;       // DECL_VARIABLE
        DeclFunction func;      // DECL_FUNCTION
    };
};

extern Declaration *decl_variable(AstState *state, char *name, Type *type, StorageClass sc, Initializer *init, FileLine loc);
extern Declaration *decl_function(AstState *state, char *name, Type *type, StorageClass sc, List parms, List body, bool has_body, FileLine loc);

//
// statemements
//
typedef enum {
    STMT_NULL,
    STMT_RETURN,
    STMT_IF,
    STMT_EXPRESSION,
    STMT_LABEL,
    STMT_GOTO,
    STMT_COMPOUND,
    STMT_WHILE,
    STMT_FOR,
    STMT_DOWHILE,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_SWITCH,
    STMT_CASE,
    STMT_DEFAULT,
} StatementTag;

typedef struct {
    Expression *exp;            // return value (may be NULL) 
} StmtReturn;

typedef struct {
    Expression *condition;      // condition to check
    Statement *thenpart;        // `then` part statement (must exist)
    Statement *elsepart;        // `else` part statement (optional)
} StmtIf;

typedef struct {
    Expression *exp;            // expression used as statement
} StmtExpression;

typedef struct {
    char *name;                 // label name
    Statement *stmt;            // labeled statement
} StmtLabel;

typedef struct {
    char *target;               // target location
} StmtGoto;

typedef struct {
    List items;                 // of <BlockItem>
} StmtCompound;

typedef enum {
    FI_NONE,
    FI_DECLARATION,
    FI_EXPRESSION,
} ForInitTag;

typedef struct {
    ForInitTag tag;
    union {
        Expression *exp;        // expression to evaluate at top of loop
        Declaration *decl;      // declaration at start of loop
    };
} ForInit;

typedef struct {
    ForInit *init;              // loop initializer
    Expression *cond;           // loop condition (optional)
    Expression *post;           // end of loop expression (optional)
    Statement *body;            // loop body
    int label;                  // loop label 
} StmtFor;

typedef struct {
    Expression *cond;           // loop condition
    Statement *body;            // loop body 
    int label;                  // loop label
} StmtWhile;

typedef struct {
    Expression *cond;           // loop condition
    Statement *body;            // loop body 
    int label;                  // loop label
} StmtDoWhile;

typedef struct {
    int label;                  // loop label
} StmtBreak;

typedef struct {
    int label;                  // loop label
} StmtContinue;

typedef struct {
    ListNode list;              // place in list
    unsigned long value;        // case value
} CaseLabel;

typedef struct {
    Expression *cond;           // switch condition
    Statement *body;            // body containing cases
    int label;                  // even though not a loop, loop label for enclosed breaks
    List cases;                 // list of cases inside this switch
    bool has_default;           // true if there is a default label
} StmtSwitch;

typedef struct {
    unsigned long value;        // case value
    int label;                  // label of enclosing switch
    Statement *stmt;            // labeled statement
} StmtCase;

typedef struct {
    int label;                  // label of enclosing switch
    Statement *stmt;            // labeled statement
} StmtDefault;

struct Statement {
    ListNode list;
    StatementTag tag;
    FileLine loc;               

    union {
        StmtReturn ret;         // STMT_RETURN
        StmtIf ifelse;          // STMT_IF
        StmtExpression exp;     // STMT_EXPRESSION
        StmtLabel label;        // STMT_LABEL
        StmtGoto goto_;         // STMT_GOTO
        StmtCompound compound;  // STMT_COMPOUND
        StmtFor for_;           // STMT_FOR
        StmtWhile while_;       // STMT_WHILE
        StmtDoWhile dowhile;    // STMT_DOWHILE
        StmtBreak break_;       // STMT_BREAK
        StmtContinue continue_; // STMT_CONTINUE
        StmtSwitch switch_;     // STMT_SWITCH
        StmtCase case_;         // STMT_CASE
        StmtDefault default_;   // STMT_DEFAULT
    };
};

extern Statement *stmt_null(AstState *state, FileLine loc);
extern Statement *stmt_return(AstState *state, Expression *exp, FileLine loc);
extern Statement *stmt_if(AstState *state, Expression *condition, Statement *thenpart, Statement *elsepart, FileLine loc);
extern Statement *stmt_expression(AstState *state, Expression *exp, FileLine loc);
extern Statement *stmt_label(AstState *state, char *name, Statement *stmt, FileLine loc);
extern Statement *stmt_goto(AstState *state, char *target, FileLine loc);
extern Statement *stmt_compound(AstState *state, List items, FileLine loc);

extern ForInit *forinit(void);
extern ForInit *forinit_exp(Expression *exp);
extern ForInit *forinit_decl(Declaration *decl);
extern void forinit_free(ForInit *fi);

extern Statement *stmt_for(AstState *state, ForInit *init, Expression *cond, Expression *post, Statement *body, FileLine loc);
extern Statement *stmt_while(AstState *state, Expression *cond, Statement *body, FileLine loc);
extern Statement *stmt_do_while(AstState *state, Expression *cond, Statement *body, FileLine loc);
extern Statement *stmt_break(AstState *state, FileLine loc);
extern Statement *stmt_continue(AstState *state, FileLine loc);
extern Statement *stmt_switch(AstState *state, Expression *cond, Statement *body, FileLine loc);
extern Statement *stmt_case(AstState *state, unsigned long value, Statement *stmt, FileLine loc);
extern Statement *stmt_default(AstState *state, Statement *stmt, FileLine loc);

//
// A block item is either a declaration or a statement.
//
typedef enum {
    BI_DECLARATION,
    BI_STATEMENT,
} BlockItemTag;

struct BlockItem {
    ListNode list;
    BlockItemTag tag;

    union {
        Declaration *decl;
        Statement *stmt;
    };
};

extern BlockItem *blki_declaration(Declaration *decl);
extern BlockItem *blki_statement(Statement *stmt);
extern void blki_free(BlockItem *blki);

//
// AST
//
typedef struct {
    FileLine loc;
    List decls;                 // A list of <Declaration>
} AstProgram;

extern AstState *ast_alloc(void);
extern void ast_free(AstState *ast);

extern AstProgram *ast_program(List decls, FileLine loc);
extern void ast_free_program(AstProgram *prog);

extern void ast_print(AstProgram *prog, bool locs);
