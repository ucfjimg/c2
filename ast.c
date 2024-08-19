#include "ast.h"

#include "ice.h"
#include "safemem.h"

#include <ctype.h>
#include <stdio.h>

static void init_free(Initializer *init);
static void init_print_recurse(Initializer *init, int tab, bool locs);
static void stmt_print_recurse(Statement *ast, int tab, bool locs);
static void exp_print_recurse(Expression *exp, int tab, bool locs);
static void decl_print_declaration(Declaration *decl, int tab, bool locs);
static void ast_print_blockitem_list(List block, int tab, bool locs);

static Expression *exp_pool_alloc(AstState *state, ExpressionTag tag, FileLine loc)
{
    Expression *exp = mp_alloc(state->exp_pool);
    exp->tag = tag;
    exp->loc = loc;
    return exp;
}

//
// Construct an integer constant expression.
//
Expression *exp_int(AstState *state, unsigned long intval, FileLine loc)
{
    Expression *exp = exp_pool_alloc(state, EXP_INT, loc);
    exp->intval = intval;
    return exp;
}

//
// Construct an long constant expression.
//
Expression *exp_long(AstState *state, unsigned long longval, FileLine loc)
{
    Expression *exp = exp_pool_alloc(state, EXP_LONG, loc);
    exp->longval = longval;
    return exp;
}

//
// Construct an unsigned integer constant expression.
//
Expression *exp_uint(AstState *state, unsigned long intval, FileLine loc)
{
    Expression *exp = exp_pool_alloc(state, EXP_UINT, loc);
    exp->intval = intval;
    return exp;
}

//
// Construct an unsigned long constant expression.
//
Expression *exp_ulong(AstState *state, unsigned long longval, FileLine loc)
{
    Expression *exp = exp_pool_alloc(state, EXP_ULONG, loc);
    exp->longval = longval;
    return exp;
}

//
// Construct a floating point constant expression.
//
Expression *exp_float(AstState *state, double floatval, FileLine loc)
{
    Expression *exp = exp_pool_alloc(state, EXP_FLOAT, loc);
    exp->floatval = floatval;
    return exp;
}

//
// Construct a string constant expression.
//
Expression *exp_string(AstState *state, char *data, size_t length, FileLine loc)
{
    Expression *exp = exp_pool_alloc(state, EXP_STRING, loc);

    exp->string.length = length;
    exp->string.data = safe_malloc(length);
    memcpy(exp->string.data, data, length);

    return exp;
}

//
// Construct a variable reference expression.
//
Expression *exp_var(AstState *state, char *name, FileLine loc)
{
    Expression *exp = exp_pool_alloc(state, EXP_VAR, loc);
    exp->var.name = safe_strdup(name);
    return exp;
}

//
// Construct a unary operator.
//
Expression *exp_unary(AstState *state, UnaryOp op, Expression *exp, FileLine loc)
{
    Expression *uexp = exp_pool_alloc(state, EXP_UNARY, loc);
    uexp->unary.op = op;
    uexp->unary.exp = exp;
    return uexp;
}

//
// Construct a binary operator.
//
Expression *exp_binary(AstState *state, BinaryOp op, Expression *left, Expression *right, FileLine loc)
{
    Expression *bexp = exp_pool_alloc(state, EXP_BINARY, loc);
    bexp->binary.op = op;
    bexp->binary.left = left;
    bexp->binary.right = right;
 
    return bexp;
}

//
// Construct a conditional operator (i.e. a ? b : c).
//
Expression *exp_conditional(AstState *state, Expression *conditional, Expression *trueval, Expression *falseval, FileLine loc)
{
    Expression *cexp = exp_pool_alloc(state, EXP_CONDITIONAL, loc);

    cexp->conditional.cond = conditional;
    cexp->conditional.trueval = trueval;
    cexp->conditional.falseval = falseval;

    return cexp;
}

//
// Construct an assignment expression.
//
Expression *exp_assignment(AstState *state, BinaryOp op, Expression *left, Expression *right, FileLine loc)
{
    ICE_ASSERT(op == BOP_ASSIGN || bop_is_compound_assign(op));
    Expression *assign = exp_pool_alloc(state, EXP_ASSIGNMENT, loc);

    assign->assignment.op = op;
    assign->assignment.left = left;
    assign->assignment.right = right;

    return assign;
}

//
// Free an assignment expression.
//
void exp_assignment_free(ExpAssignment *assign)
{
}

//
// Construct a function call.
//
Expression *exp_function_call(AstState *state, char *name, List args, FileLine loc)
{
    Expression *call = exp_pool_alloc(state, EXP_FUNCTION_CALL, loc);

    call->call.name = safe_strdup(name);
    call->call.args = args;

    return call;
}

//
// Construct a cast operator.
//
Expression *exp_cast(AstState *state, Type *type, Expression *exp, FileLine loc)
{
    Expression *cast = exp_pool_alloc(state, EXP_CAST, loc);

    cast->cast.type = type;
    cast->cast.exp = exp;

    return cast;
}

//
// Construct a deref operator.
//
Expression *exp_deref(AstState *state, Expression *exp, FileLine loc)
{
    Expression *deref = exp_pool_alloc(state, EXP_DEREF, loc);
    deref->deref.exp = exp;
    return deref;
}

//
// Construct an addrof operator.
//
Expression *exp_addrof(AstState *state, Expression *exp, FileLine loc)
{
    Expression *addrof = exp_pool_alloc(state, EXP_ADDROF, loc);
    addrof->addrof.exp = exp;
    return addrof;
}

//
// Construct a subscript operator.
//
Expression *exp_subscript(AstState *state, Expression *left, Expression *right, FileLine loc)
{
    Expression *subs = exp_pool_alloc(state, EXP_SUBSCRIPT, loc);

    subs->subscript.left = left;
    subs->subscript.right = right;   

    return subs;
}

//
// Set the type annotation of an expression node, replacing any previous type.
//
void exp_set_type(Expression *exp, Type *type)
{
    if (exp->type) {
        type_free(exp->type);
    }
    exp->type = type;
}

//
// Return true if the given expression represents a constant value.
//
bool exp_is_constant(Expression *exp)
{
    switch (exp->tag) {
        case EXP_INT:
        case EXP_LONG:
        case EXP_UINT:
        case EXP_ULONG:
        case EXP_FLOAT:             
        case EXP_STRING:            return true;

        case EXP_VAR:
        case EXP_UNARY:
        case EXP_BINARY:
        case EXP_CONDITIONAL:
        case EXP_ASSIGNMENT:
        case EXP_FUNCTION_CALL:
        case EXP_CAST:
        case EXP_DEREF:
        case EXP_ADDROF:
        case EXP_SUBSCRIPT:         return false;
    }

    ICE_ASSERT(((void)"invalid expression tag in exp_is_constant", false));
    return false;
}

//
// Return true if the given expression represents an integer constant value.
//
bool exp_is_int_constant(Expression *exp)
{
    switch (exp->tag) {
        case EXP_INT:
        case EXP_LONG:
        case EXP_UINT:
        case EXP_ULONG:             return true;
        
        case EXP_FLOAT:
        case EXP_STRING:
        case EXP_VAR:
        case EXP_UNARY:
        case EXP_BINARY:
        case EXP_CONDITIONAL:
        case EXP_ASSIGNMENT:
        case EXP_FUNCTION_CALL:
        case EXP_CAST:
        case EXP_DEREF:
        case EXP_ADDROF:
        case EXP_SUBSCRIPT:         return false;
    }

    ICE_ASSERT(((void)"invalid expression tag in exp_is_constant", false));
    return false;
}

//
// Constructor for a single initializer.
//
Initializer *init_single(Expression *exp)
{
    Initializer *init = safe_zalloc(sizeof(Initializer));

    init->tag = INIT_SINGLE;
    init->single = exp;

    return init;
}

//
// Constructor for a compound initializer.
//
Initializer *init_compound(List inits)
{
    Initializer *init = safe_zalloc(sizeof(Initializer));

    init->tag = INIT_COMPOUND;
    init->compound = inits;

    return init;
}

//
// Free a compound initializer
//
static void init_compound_free(List inits)
{
    ListNode *next = NULL;
    for (ListNode *curr = inits.head; curr; curr = next) {
        next = curr->next;
        init_free(CONTAINER_OF(curr, Initializer, list));
    }
}

//
// Free an initializer.
//
static void init_free(Initializer *init)
{
    switch (init->tag) {
        case INIT_SINGLE:   break;
        case INIT_COMPOUND: init_compound_free(init->compound); break;
    }
}

//
// Constructor for a variable declaration.
//
// Note that `init` is optional and will be NULL if the declaration
// has no initializer.
// 
Declaration *decl_variable(AstState *state, char *name, Type *type, StorageClass sc, Initializer *init, FileLine loc)
{
    Declaration *decl = mp_alloc(state->decl_pool);

    decl->tag = DECL_VARIABLE;
    decl->loc = loc;
    decl->var.type = type;
    decl->var.storage_class = sc;
    decl->var.name = safe_strdup(name);
    decl->var.init = init;

    return decl;
}

//
// Constructor for a function parameter. 
//
FuncParameter *func_parm(char *name)
{
    FuncParameter *parm = safe_zalloc(sizeof(FuncParameter));
    parm->name = safe_strdup(name);
    return parm;
}

//
// Free a function parameter.
//
void func_parm_free(FuncParameter *parm)
{
    safe_free(parm->name);
    safe_free(parm);
}

//
// Constructor for a function declaration.
//
Declaration *decl_function(AstState *state, char *name, Type *type, StorageClass sc, List parms, List body, bool has_body, FileLine loc)
{
    Declaration *decl = mp_alloc(state->decl_pool);

    decl->tag = DECL_FUNCTION;
    decl->loc = loc;
    decl->func.type = type;
    decl->func.storage_class = sc;
    decl->func.name = safe_strdup(name);
    decl->func.parms = parms;
    decl->func.body = body;
    decl->func.has_body = has_body;

    return decl;
}

//
// Allocator for all statement objects.
//
static Statement *stmt_alloc(AstState *state, StatementTag tag, FileLine loc)
{
    Statement *stmt = mp_alloc(state->stmt_pool);
    stmt->tag = tag;
    stmt->loc = loc;
    return stmt;
}

//
// Construct a null statement.
//
Statement *stmt_null(AstState *state, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_NULL, loc);
    return stmt;
}

//
// Construct a return statement around an expression (which may be NULL).
//
Statement *stmt_return(AstState *state, Expression *exp, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_RETURN, loc);
    stmt->ret.exp = exp;
    return stmt;
}

//
// Construct an if statement.
//
Statement *stmt_if(AstState *state, Expression *condition, Statement *thenpart, Statement *elsepart, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_IF, loc);
    stmt->ifelse.condition = condition;
    stmt->ifelse.thenpart = thenpart;
    stmt->ifelse.elsepart = elsepart;

    return stmt;
}

//
// Construct an expression used as a statement.
//
Statement *stmt_expression(AstState *state, Expression *exp, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_EXPRESSION, loc);
    stmt->exp.exp = exp;
    return stmt;
}

//
// Constructor for a label statment (e.g. `label:`).
//
Statement *stmt_label(AstState *state, char *name, Statement *labeled_stmt, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_LABEL, loc);
    stmt->label.name = safe_strdup(name);
    stmt->label.stmt = labeled_stmt;
    return stmt;
}

//
// Constructor for a goto statement.
//
Statement *stmt_goto(AstState *state, char *target, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_GOTO, loc);
    stmt->goto_.target = safe_strdup(target);
    return stmt;
}

//
// Constructor for a compound statement.
//
Statement *stmt_compound(AstState *state, List items, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_COMPOUND, loc);
    stmt->compound.items = items;
    return stmt;
}

//
// Constructor for an empty for initializer.
//
ForInit *forinit(void)
{
    ForInit *fi = safe_zalloc(sizeof(ForInit));
    fi->tag = FI_NONE;
    return fi;
}

//
// Constructor for a for initializer with a expression.
//
ForInit *forinit_exp(Expression *exp)
{
    ForInit *fi = forinit();

    fi->tag = FI_EXPRESSION;
    fi->exp = exp;

    return fi;
}

//
// Constructor for a for initializer with a declaration.
//
ForInit *forinit_decl(Declaration *decl)
{
    ForInit *fi = forinit();

    fi->tag = FI_DECLARATION;
    fi->decl = decl;

    return fi;
}

//
// Free a for loop initializer.
//
void forinit_free(ForInit *fi)
{
    if (fi) {
        switch (fi->tag) {
            case FI_EXPRESSION:     break;
            case FI_DECLARATION:    break;
            case FI_NONE:           break;
        }
    }
}

//
// Constuctor for a for loop.
//
Statement *stmt_for(AstState *state, ForInit *init, Expression *cond, Expression *post, Statement *body, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_FOR, loc);

    stmt->for_.init = init;
    stmt->for_.cond = cond;
    stmt->for_.post = post;
    stmt->for_.body = body;
    stmt->for_.label = -1;

    return stmt;
}

//
// Constructor for a while loop.
//
Statement *stmt_while(AstState *state, Expression *cond, Statement *body, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_WHILE, loc);

    stmt->while_.cond = cond;
    stmt->while_.body = body;
    stmt->while_.label = -1;

    return stmt;
}


//
// Constructor for a do-while loop.
//
Statement *stmt_do_while(AstState *state, Expression *cond, Statement *body, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_DOWHILE, loc);

    stmt->dowhile.cond = cond;
    stmt->dowhile.body = body;
    stmt->dowhile.label = -1;

    return stmt;
}

//
// Constructor for a break statement.
//
Statement *stmt_break(AstState *state, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_BREAK, loc);
    stmt->break_.label = -1;
    return stmt;
}

//
// Constructor for a continue statement.
//
Statement *stmt_continue(AstState *state, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_CONTINUE, loc);
    stmt->continue_.label = -1;
    return stmt;
}

//
// Constructor for a switch statement.
//
Statement *stmt_switch(AstState *state, Expression *cond, Statement *body, FileLine loc)
{
    Statement *stmt = stmt_alloc(state, STMT_SWITCH, loc);
    stmt->switch_.cond = cond;
    stmt->switch_.body = body;
    stmt->switch_.label = -1;
    return stmt;
}

//
// Constructor for a case statement.
//
Statement *stmt_case(AstState *state, unsigned long value, Statement *stmt, FileLine loc)
{
    Statement *casestmt = stmt_alloc(state, STMT_CASE, loc);
    casestmt->case_.value = value;
    casestmt->case_.stmt = stmt;
    return casestmt;
}

//
// Constructor for a default statement.
//
Statement *stmt_default(AstState *state, Statement *stmt, FileLine loc)
{
    Statement *defstmt = stmt_alloc(state, STMT_DEFAULT, loc);
    defstmt->default_.label = -1;
    defstmt->default_.stmt = stmt;
    return defstmt;
}

//
// Allocator for all BlockItem nodes.
//
static BlockItem *blki_alloc(BlockItemTag tag)
{
    BlockItem *blki = safe_zalloc(sizeof(BlockItem));
    blki->tag = tag;
    return blki;
}

//
// Constructor for a declaration block item.
//
BlockItem *blki_declaration(Declaration *decl)
{
    BlockItem *blki = blki_alloc(BI_DECLARATION);
    blki->decl = decl;
    return blki;
}

//
// Constructor for a statement block item.
//
BlockItem *blki_statement(Statement *stmt)
{
    BlockItem *blki = blki_alloc(BI_STATEMENT);
    blki->stmt = stmt;
    return blki;
}

//
// Construct a program.
//
AstProgram *ast_program(List decls, FileLine loc)
{
    AstProgram *prog = safe_zalloc(sizeof(AstProgram));
    prog->loc = loc;
    prog->decls = decls;
    return prog;
}

//
// Free a program.
//
void ast_free_program(AstProgram *prog)
{
    if (prog) {
        ListNode *next = NULL;
        for (ListNode *curr = prog->decls.head; curr; curr = next) {
            next = curr->next;
        }
        safe_free(prog);
    }
}

//
// Print an integer constant expression.
//
static void print_exp_const_int(unsigned long val, int tab)
{
    printf("const-int(%lu);\n", val);
}

//
// Print a long constant expression.
//
static void print_exp_const_long(unsigned long val, int tab)
{
    printf("const-long(%lu);\n", val);
}

//
// Print an unsigned integer constant expression.
//
static void print_exp_const_uint(unsigned long val, int tab)
{
    printf("const-uint(%lu);\n", val);
}

//
// Print an unsgined long constant expression.
//
static void print_exp_const_ulong(unsigned long val, int tab)
{
    printf("const-ulong(%lu);\n", val);
}

//
// Print a floating point constant expression.
//
static void print_exp_const_float(double val, int tab)
{
    printf("const-float(%g);\n", val);
}

//
// Print a string constant expression.
//
static void print_exp_const_string(ExpString *string, int tab)
{
    printf("string(\"");

    for (size_t i = 0; i < string->length; i++) {
        char ch = string->data[i];

        if (isprint(ch)) {
            printf("%c", ch);
        } else {
            switch (ch) {
                case '\0': printf("\\0"); break;
                case '\a': printf("\\a"); break;
                case '\b': printf("\\b"); break;
                case '\f': printf("\\f"); break;
                case '\n': printf("\\n"); break;
                case '\r': printf("\\r"); break;
                case '\t': printf("\\t"); break;
                case '\v': printf("\\v"); break;
                default:
                    printf("\\x%02x", ch & 0xff);
            }
        }
    }

    printf("\");\n");
}

//
// Print an variable reference expression.
//
static void print_exp_var(char *name, int tab)
{
    printf("var(%s);\n", name);
}

//
// Print a unary expression.
//
static void print_exp_unary(ExpUnary *unary, int tab, bool locs)
{
    printf("unary(%s) {\n", uop_describe(unary->op));
    exp_print_recurse(unary->exp, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a binary expression.
//
static void print_exp_binary(ExpBinary *binary, int tab, bool locs)
{
    printf("binary(%s) {\n", bop_describe(binary->op));
    exp_print_recurse(binary->left, tab + 2, locs);
    printf("%*s  ,\n", tab, "");
    exp_print_recurse(binary->right, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a conditional expression.
//
static void print_exp_conditional(ExpConditional *cond, int tab, bool locs)
{
    printf("conditional {\n");
    exp_print_recurse(cond->cond, tab + 2, locs);
    printf("%*s} ? {\n", tab, "");
    exp_print_recurse(cond->trueval, tab + 2, locs);
    printf("%*s} : {\n", tab, "");
    exp_print_recurse(cond->falseval, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print an assignment expression.
//
static void print_exp_assignment(ExpAssignment *assign, int tab, bool locs)
{
    printf("assignment(%s) {\n", bop_describe(assign->op));
    exp_print_recurse(assign->left, tab + 2, locs);
    printf("%*s}, {\n", tab, "");
    exp_print_recurse(assign->right, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a function call expression.
//
static void print_exp_function_call(ExpFunctionCall *call, int tab, bool locs)
{
    printf("call(%s) {\n", call->name);
    
    for (ListNode *curr = call->args.head; curr; curr = curr->next) {
        Expression *arg = CONTAINER_OF(curr, Expression, list);
        exp_print_recurse(arg, tab + 2, locs);
    }

    printf("%*s}\n", tab, "");
}

//
// Print a cast expression.
//
static void print_exp_cast(ExpCast *cast, int tab, bool locs)
{
    char *type = type_describe(cast->type);
    printf("cast(%s) {\n", type);
    safe_free(type);

    exp_print_recurse(cast->exp, tab + 2, locs);

    printf("%*s}\n", tab, "");
}

//
// Print a deref expression.
//
static void print_exp_deref(ExpDeref *deref, int tab, bool locs)
{
    printf("deref {\n");
    exp_print_recurse(deref->exp, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print an addrof expression.
//
static void print_exp_addrof(ExpAddrOf *addrof, int tab, bool locs)
{
    printf("addrof {\n");
    exp_print_recurse(addrof->exp, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a subscript expression.
//
static void print_exp_subscript(ExpSubscript *subs, int tab, bool locs)
{
    printf("subscript {\n");
    exp_print_recurse(subs->left, tab + 2, locs);
    printf("%*s}, {\n", tab, "");
    exp_print_recurse(subs->right, tab + 2, locs);    
    printf("%*s}\n", tab, "");
}

//
// Recusively print an expression, starting at indent `tab`
//
static void exp_print_recurse(Expression *exp, int tab, bool locs)
{
    if (locs) {
        char *loc = fileline_describe(&exp->loc);
        printf("%*s/* %s */\n", tab, "", loc);
        safe_free(loc);
    }

    printf("%*s", tab, "");
    if (exp->type) {
        char *type = type_describe(exp->type);
        printf("(%s) ", type);
        safe_free(type);
    }

    switch (exp->tag) {
        case EXP_INT:           print_exp_const_int(exp->intval, tab); break;
        case EXP_LONG:          print_exp_const_long(exp->longval, tab); break;
        case EXP_UINT:          print_exp_const_uint(exp->intval, tab); break;
        case EXP_ULONG:         print_exp_const_ulong(exp->longval, tab); break;
        case EXP_FLOAT:         print_exp_const_float(exp->floatval, tab); break;
        case EXP_STRING:        print_exp_const_string(&exp->string, tab); break;
        case EXP_VAR:           print_exp_var(exp->var.name, tab); break;
        case EXP_UNARY:         print_exp_unary(&exp->unary, tab, locs); break;
        case EXP_BINARY:        print_exp_binary(&exp->binary, tab, locs); break;
        case EXP_CONDITIONAL:   print_exp_conditional(&exp->conditional, tab, locs); break;
        case EXP_ASSIGNMENT:    print_exp_assignment(&exp->assignment, tab, locs); break;
        case EXP_FUNCTION_CALL: print_exp_function_call(&exp->call, tab, locs); break;
        case EXP_CAST:          print_exp_cast(&exp->cast, tab, locs); break;
        case EXP_DEREF:         print_exp_deref(&exp->deref, tab, locs); break;
        case EXP_ADDROF:        print_exp_addrof(&exp->addrof, tab, locs); break;
        case EXP_SUBSCRIPT:     print_exp_subscript(&exp->subscript, tab, locs); break;
    }
}

//
// Return a static storage class decription with a trailing space.
//
static char *storage_class_describe(StorageClass sc)
{
    switch (sc) {
        case SC_EXTERN: return "extern ";
        case SC_STATIC: return "static ";
        case SC_NONE:   break;
    }

    return "";
}

//
// Print a single item initializer.
//
static void init_print_single(Initializer *init, int tab, bool locs)
{
    char *desc = init->type ? type_describe(init->type) : safe_strdup("<no-type>");
    printf("%*ssingle-init (%s) {\n", tab, "", desc);
    safe_free(desc);
    exp_print_recurse(init->single, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a compound initializer.
//
static void init_print_compound(Initializer *init, int tab, bool locs)
{
    char *desc = init->type ? type_describe(init->type) : safe_strdup("<no-type>");
    printf("%*scompound-init (%s) {\n", tab, "", desc);
    safe_free(desc);

    for (ListNode *curr = init->compound.head; curr; curr = curr->next) {
        init_print_recurse(CONTAINER_OF(curr, Initializer, list), tab + 2, locs);
        if (curr->next) {
            printf("%*s,\n", tab + 2, "");
        }
    }

    printf("%*s}\n", tab, "");
}

//
// Print an initializer.
//
static void init_print_recurse(Initializer *init, int tab, bool locs)
{
    switch (init->tag) {
        case INIT_SINGLE:   init_print_single(init, tab, locs); break;
        case INIT_COMPOUND: init_print_compound(init, tab, locs); break;
    }
}

//
// Print a variable declaration.
//
static void decl_print_variable(DeclVariable *var, int tab, bool locs)
{
    char *type = type_describe(var->type);
    printf("%*sdeclare-var(%s %s%s)", tab, "", type, storage_class_describe(var->storage_class), var->name);
    safe_free(type);
    if (var->init) {
        printf(" = {\n");
        init_print_recurse(var->init, tab + 2, locs);
        printf("%*s}", tab, "");
    }
    printf(";\n");
}

//
// Print a function declaration.
//
static void decl_print_function(DeclFunction *func, int tab, bool locs)
{
    printf("%*sdeclare-func(%s%s) (", tab, "", storage_class_describe(func->storage_class), func->name);

    ListNode *pcurr = func->parms.head;
    ListNode *tcurr = func->type->func.parms.head;

    for (; pcurr && tcurr; pcurr = pcurr->next, tcurr = tcurr->next) {
        FuncParameter *parm = CONTAINER_OF(pcurr, FuncParameter, list);
        TypeFuncParam *ptype = CONTAINER_OF(tcurr, TypeFuncParam, list);
        
        char *type = type_describe(ptype->parmtype);
        printf("%s %s", type, parm->name);
        safe_free(type);

        if (pcurr->next) {
            printf(", ");
        }
    }
    
    char *type = type_describe(func->type->func.ret);
    printf(") -> %s", type);
    safe_free(type);

    if (func->body.head) {
        printf(" {\n");
        ast_print_blockitem_list(func->body, tab + 2, locs);
        printf("%*s}", tab, "");
    } else {
        printf(";");
    }

    printf("\n");
}

//
// Print a declaration.
//
static void decl_print_declaration(Declaration *decl, int tab, bool locs)
{
    switch (decl->tag) {
        case DECL_FUNCTION: decl_print_function(&decl->func, tab, locs); break;
        case DECL_VARIABLE: decl_print_variable(&decl->var, tab, locs); break;
    }
}

//
// Print a null statement.
//
static void stmt_print_null(int tab)
{
    printf("%*snull-statement;\n", tab, "");
}

//
// Print a return statement.
//
static void stmt_print_return(StmtReturn *ret, int tab, bool locs)
{
    printf("%*sreturn {\n", tab, "");
    exp_print_recurse(ret->exp, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print an if statement.
//
static void stmt_print_if(StmtIf *ifelse, int tab, bool locs)
{
    printf("%*sif (\n", tab, "");
    exp_print_recurse(ifelse->condition, tab + 2, locs);
    printf("%*s) {\n", tab, "");
    stmt_print_recurse(ifelse->thenpart, tab + 2, locs);
    if (ifelse->elsepart) {
        printf("%*s} else {\n", tab, "");
        stmt_print_recurse(ifelse->elsepart, tab + 2, locs);
    }
    printf("%*s}\n", tab, "");
}

//
// Print a statement expression.
//
static void stmt_print_expression(StmtExpression *exp, int tab, bool locs)
{
    printf("%*sexp {\n", tab, "");
    exp_print_recurse(exp->exp, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a label statement.
//
static void stmt_print_label(StmtLabel *label, int tab, bool locs)
{
    printf("%*slabel(%s) {\n", tab, "", label->name);
    stmt_print_recurse(label->stmt, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a goto statement.
//
static void stmt_print_goto(StmtGoto *goto_, int tab)
{
    printf("%*sgoto(%s);\n", tab, "", goto_->target);
}

//
// Print a compound statement.
//
static void stmt_print_compound(StmtCompound *compound, int tab, bool locs)
{
    printf("%*scompound {\n", tab, "");
    ast_print_blockitem_list(compound->items, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a for initializer.
//
static void stmt_print_forinit(ForInit *fi, int tab, bool locs)
{
    switch (fi->tag) {
        case FI_EXPRESSION:     exp_print_recurse(fi->exp, tab, locs); break;
        case FI_DECLARATION:    decl_print_declaration(fi->decl, tab, locs); break;
        case FI_NONE:           break;
    }
}

//
// Print a for loop.
//
static void stmt_print_for(StmtFor *for_, int tab, bool locs)
{
    printf("%*sfor(%d) {\n", tab, "", for_->label);
    stmt_print_forinit(for_->init, tab + 2, locs);
    printf("%*s} ; {\n", tab, "");
    if (for_->cond) {
        exp_print_recurse(for_->cond, tab + 2, locs);
    }
    printf("%*s} ; {\n", tab, "");
    if (for_->post) {
        exp_print_recurse(for_->post, tab + 2, locs);
    }
    printf("%*s} {\n", tab, "");
    stmt_print_recurse(for_->body, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a while loop.
//
static void stmt_print_while(StmtWhile *while_, int tab, bool locs)
{
    printf("%*swhile(%d) {\n", tab, "", while_->label);
    exp_print_recurse(while_->cond, tab + 2, locs);
    printf("%*s} {\n", tab, "");
    stmt_print_recurse(while_->body, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a do-while loop.
//
static void stmt_print_do_while(StmtDoWhile *dowhile, int tab, bool locs)
{
    printf("%*sdo(%d) {\n", tab, "", dowhile->label);
    stmt_print_recurse(dowhile->body, tab + 2, locs);
    printf("%*s} while {\n", tab, "");
    exp_print_recurse(dowhile->cond, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a break statement.
//
static void stmt_print_break(StmtBreak *break_, int tab)
{
    printf("%*sbreak(%d);\n", tab, "", break_->label);
}

//
// Print a continue statement.
//
static void stmt_print_continue(StmtContinue *continue_, int tab)
{
    printf("%*scontinue(%d);\n", tab, "", continue_->label);
}

//
// Print a switch statement.
//
static void stmt_print_switch(StmtSwitch *switch_, int tab, bool locs)
{
    printf("%*sswitch(%d) {\n", tab, "", switch_->label);
    exp_print_recurse(switch_->cond, tab + 2, locs);
    printf("%*s} {\n", tab, "");
    stmt_print_recurse(switch_->body, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a case statement.
//
static void stmt_print_case(StmtCase *case_, int tab, bool locs)
{
    printf("%*scase (label=%d,value=%ld) {\n", tab, "", case_->label, case_->value);
    stmt_print_recurse(case_->stmt, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Print a default statement.
//
static void stmt_print_default(StmtDefault *def, int tab, bool locs)
{
    printf("%*sdefault {\n", tab, "");
    stmt_print_recurse(def->stmt, tab + 2, locs);
    printf("%*s}\n", tab, "");
}

//
// Recusively print a statement, starting at indent `tab`
//
static void stmt_print_recurse(Statement *stmt, int tab, bool locs)
{
    if (locs) {
        char *loc = fileline_describe(&stmt->loc);
        printf("%*s/* %s */\n", tab, "", loc);
        safe_free(loc);
    }

    switch (stmt->tag) {
        case STMT_NULL:         stmt_print_null(tab); break;
        case STMT_RETURN:       stmt_print_return(&stmt->ret, tab, locs); break;
        case STMT_IF:           stmt_print_if(&stmt->ifelse, tab, locs); break;
        case STMT_EXPRESSION:   stmt_print_expression(&stmt->exp, tab, locs); break;
        case STMT_LABEL:        stmt_print_label(&stmt->label, tab, locs); break;
        case STMT_GOTO:         stmt_print_goto(&stmt->goto_, tab); break;
        case STMT_COMPOUND:     stmt_print_compound(&stmt->compound, tab, locs); break;
        case STMT_FOR:          stmt_print_for(&stmt->for_, tab, locs); break;
        case STMT_WHILE:        stmt_print_while(&stmt->while_, tab, locs); break;
        case STMT_DOWHILE:      stmt_print_do_while(&stmt->dowhile, tab, locs); break;
        case STMT_BREAK:        stmt_print_break(&stmt->break_, tab); break;
        case STMT_CONTINUE:     stmt_print_continue(&stmt->continue_, tab); break;
        case STMT_SWITCH:       stmt_print_switch(&stmt->switch_, tab, locs); break;
        case STMT_CASE:         stmt_print_case(&stmt->case_, tab, locs); break;
        case STMT_DEFAULT:      stmt_print_default(&stmt->default_, tab, locs); break;
    }
}

//
// Print a block (list of BlockItem's).
//
static void ast_print_blockitem_list(List block, int tab, bool locs)
{
    for (ListNode *curr = block.head; curr; curr = curr->next) {
        BlockItem *blki = CONTAINER_OF(curr, BlockItem, list);
        switch (blki->tag) {
            case BI_DECLARATION:    decl_print_declaration(blki->decl, tab + 2, locs); break;
            case BI_STATEMENT:      stmt_print_recurse(blki->stmt, tab + 2, locs); break;
        }
    }
}

//
// Recursively print an AST. If `locs` is true, also print the file/line
// location of each node.
//
void ast_print(AstProgram *prog, bool locs)
{
    printf("program() {\n");
    for (ListNode *curr = prog->decls.head; curr; curr = curr->next) {
        Declaration *decl = CONTAINER_OF(curr, Declaration, list);
        decl_print_declaration(decl, 2, locs);
    }
    printf("}\n");
}

//
// Allocate an AST state with pools for the different node types.
//
AstState *ast_alloc(void)
{
    AstState *state = safe_zalloc(sizeof(AstState));

    state->exp_pool = mp_create(sizeof(Expression));
    state->decl_pool = mp_create(sizeof(Declaration));
    state->stmt_pool = mp_create(sizeof(Statement));

    return state;
}

//
// Free an AST state, which also invalidates all nodes.
//
void ast_free(AstState *state)
{
    if (state) {
        mp_destroy(state->exp_pool);
        mp_destroy(state->decl_pool);
        mp_destroy(state->stmt_pool);
    }
}
