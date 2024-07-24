#include "resolve.h"

#include "errors.h"
#include "hashtab.h"
#include "ice.h"
#include "list.h"
#include "safemem.h"

typedef struct {
    HashNode hash;
    char *new_name;         // mapped, unique name -- NULL if not yet mapped
    bool from_curr_scope;   // if true, name was declared in the current scope
} VarMapNode;

typedef struct {
    HashTable *table;
} ResolveState;

static void ast_resolve_expression(ResolveState *state, Expression *exp);
static void ast_resolve_statement(ResolveState *state, Statement *stmt);

//
// Allocate a hash node for the resolve table.
//
static HashNode *restab_alloc_mapnode(void)
{
    VarMapNode *node = safe_zalloc(sizeof(VarMapNode));
    return &node->hash;
}

//
// Free a hash node from the variable table.
//
static void restab_free_mapnode(HashNode *node)
{
    safe_free(node);
}

//
// Initialize variable map table.
//
static void restab_init(ResolveState *state)
{
    state->table = hashtab_alloc(restab_alloc_mapnode, restab_free_mapnode);
}

//
// Create a new state for a new scope. All of the names declared
// outside this scope will be marked as such.
//
static void restab_new_scope(ResolveState *curr, ResolveState *next)
{
    restab_init(next);

    HashIterator iter;
    for (HashNode *node = hashtab_first(curr->table, &iter); node; node = hashtab_next(&iter)) {
        HashNode *newnode = hashtab_lookup(next->table, node->key);

        VarMapNode *currmap = CONTAINER_OF(node, VarMapNode, hash);        
        VarMapNode *nextmap = CONTAINER_OF(newnode, VarMapNode, hash);    

        nextmap->from_curr_scope = false;
        nextmap->new_name = safe_strdup(currmap->new_name);    
    }
}

//
// Look up a variable.
//
static VarMapNode *restab_get(ResolveState *state, char *name)
{
    HashNode *node = hashtab_lookup(state->table, name);
    return CONTAINER_OF(node, VarMapNode, hash);    
}

//
// Free the variable table
//
static void restab_free(ResolveState *state)
{
    hashtab_free(state->table);
}

//
// Generate a unique name for a variable. The returned string is allocated.
//
static char *ast_unique_varname(char *name)
{
    static int suffix = 0;
    return saprintf("%s.%d", name, suffix++);
}

//
// Check a declaration to see if duplicates another declaration in this scope;
// if so, report an error. Otherwise, generate a new unique name for the 
// declaration.
//
static void ast_resolve_declaration(ResolveState *state, Declaration *decl)
{
    VarMapNode *mapnode = restab_get(state, decl->name);
    if (mapnode->new_name && mapnode->from_curr_scope) {
        err_report(EC_ERROR, &decl->loc, "variable `%s` has already been declared.", decl->name);
    } else {
        safe_free(mapnode->new_name);
        mapnode->new_name = ast_unique_varname(decl->name);
        mapnode->from_curr_scope = true;
    }

    safe_free(decl->name);
    decl->name = safe_strdup(mapnode->new_name);

    if (decl->init) {
        ast_resolve_expression(state, decl->init);
    }
}

//
// Resolve a variable reference.
//
static void ast_resolve_var_reference(ResolveState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_VAR);
    ExpVar *var = &exp->var;

    VarMapNode *mapnode = restab_get(state, var->name);
    if (mapnode->new_name == NULL) {
        err_report(EC_ERROR, &exp->loc, "variable `%s` used without being declared.", var->name);
        mapnode->new_name = ast_unique_varname(var->name);
    }

    safe_free(var->name);
    var->name = safe_strdup(mapnode->new_name);
}

//
// Resolve variables in a binary expression.
//
static void ast_resolve_binary_exp(ResolveState *state, ExpBinary *bin)
{
    ast_resolve_expression(state, bin->left);
    ast_resolve_expression(state, bin->right);
}

//
// Resolve variables in a unary expression.
//
static void ast_resolve_unary_exp(ResolveState *state, ExpUnary *unary)
{
    if (
        (unary->op == UOP_PREINCREMENT || 
         unary->op == UOP_PREDECREMENT) && 
        unary->exp->tag != EXP_VAR) {
        err_report(EC_ERROR, &unary->exp->loc, "target of increment/decrement must be an l-value.");
    }
    ast_resolve_expression(state, unary->exp);
}

//
// Resolve variables in a conditional expression.
//
static void ast_resolve_conditional_exp(ResolveState *state, ExpConditional *cond)
{
    ast_resolve_expression(state, cond->cond);
    ast_resolve_expression(state, cond->trueval);
    ast_resolve_expression(state, cond->falseval);
}

//
// Resolve variables in an assignment expression.
//
static void ast_resolve_assign_exp(ResolveState *state, ExpAssignment *assign)
{
    if (assign->left->tag != EXP_VAR) {
        err_report(EC_ERROR, &assign->left->loc, "target of assignment must be an l-value.");
    }
    ast_resolve_expression(state, assign->left);
    ast_resolve_expression(state, assign->right);
}

//
// Resolve variable references in an expression.
//
static void ast_resolve_expression(ResolveState *state, Expression *exp)
{
    switch (exp->tag) {
        case EXP_VAR:           ast_resolve_var_reference(state, exp); break;
        case EXP_BINARY:        ast_resolve_binary_exp(state, &exp->binary); break;
        case EXP_UNARY:         ast_resolve_unary_exp(state, &exp->unary); break;
        case EXP_CONDITIONAL:   ast_resolve_conditional_exp(state, &exp->conditional); break;
        case EXP_ASSIGNMENT:    ast_resolve_assign_exp(state, &exp->assignment); break;
        case EXP_INT:           break;
    }
}

//
// Resolve variables in a return statement.
//
static void ast_resolve_return_stmt(ResolveState *state, StmtReturn *ret)
{
    if (ret->exp) {
        ast_resolve_expression(state, ret->exp);
    }
}

//
// Resolve variables in an if statement.
//
static void ast_resolve_if_stmt(ResolveState *state, StmtIf *ifelse)
{
    ast_resolve_expression(state, ifelse->condition);
    ast_resolve_statement(state, ifelse->thenpart);
    if (ifelse->elsepart) {
        ast_resolve_statement(state, ifelse->elsepart);
    }
}

//
// Resolve variables in a labelled statement.
//
static void ast_resolve_label(ResolveState *state, StmtLabel *label)
{
    ast_resolve_statement(state, label->stmt);
}

//
// Resolve variables in a block.
//
static void ast_resolve_block(ResolveState *state, List items)
{
    for (ListNode *curr = items.head; curr; curr = curr->next) {
        BlockItem *blki = CONTAINER_OF(curr, BlockItem, list);
        switch (blki->tag) {
            case BI_DECLARATION:    ast_resolve_declaration(state, blki->decl); break;
            case BI_STATEMENT:      ast_resolve_statement(state, blki->stmt); break;
        }
    }
}

//
// Resolve variables in a compound statement.
//
static void ast_resolve_compound(ResolveState *state, StmtCompound *compound)
{
    ResolveState newstate;

    restab_new_scope(state, &newstate);
    ast_resolve_block(&newstate, compound->items);
    restab_free(&newstate);
}

//
// Resolve variables for a statement.
//
static void ast_resolve_statement(ResolveState *state, Statement *stmt)
{
    switch (stmt->tag) {
        case STMT_EXPRESSION:   ast_resolve_expression(state, stmt->exp.exp); break;
        case STMT_RETURN:       ast_resolve_return_stmt(state, &stmt->ret); break;
        case STMT_IF:           ast_resolve_if_stmt(state, &stmt->ifelse); break;
        case STMT_NULL:         break;
        case STMT_LABEL:        ast_resolve_label(state, &stmt->label); break;
        case STMT_GOTO:         break;
        case STMT_COMPOUND:     ast_resolve_compound(state, &stmt->compound); break;
    }
}

//
// Resolve variables for one function.
//
static void ast_resolve_function(ResolveState *state, AstFunction *func)
{
    ast_resolve_block(state, func->stmts);
}

//
// Walk an AST and
// - ensure that variable usage and declarations are semantically ok
//   - variables are declared before use
//   - variables are not declared more than once in the same scope
// - rename variables to unique names within the entire program
//
void ast_resolve(AstNode *ast)
{
    ICE_ASSERT(ast->tag == AST_PROGRAM);

    ResolveState state;
    restab_init(&state);
    ast_resolve_function(&state, &ast->prog.func->func);
    restab_free(&state);
}