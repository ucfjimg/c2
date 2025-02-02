#include "resolve.h"

#include "errors.h"
#include "hashtab.h"
#include "ice.h"
#include "list.h"
#include "safemem.h"
#include "temporary.h"

#include <stdbool.h>

typedef struct {
    HashNode hash;
    char *new_name;         // mapped, unique name -- NULL if not yet mapped
    bool from_curr_scope;   // if true, name was declared in the current scope
    bool has_linkage;       // if true, identifier has external linkage
} IdentifierMapNode;

typedef struct {
    HashTable *vartab;
    HashTable *structtab;
} ResolveState;

static void ast_resolve_type(ResolveState *state, Type *type, FileLine loc);
static void ast_resolve_expression(ResolveState *state, Expression *exp);
static void ast_resolve_statement(ResolveState *state, Statement *stmt);
static void ast_resolve_function(ResolveState *state, Declaration *decl, bool global);
static void ast_resolve_struct(ResolveState *state, Declaration *decl);

//
// Allocate a hash node for the resolve table.
//
static HashNode *restab_alloc_mapnode(void)
{
    IdentifierMapNode *node = safe_zalloc(sizeof(IdentifierMapNode));
    return &node->hash;
}

//
// Free a hash node from the variable table.
//
static void restab_free_mapnode(HashNode *node)
{
    IdentifierMapNode *mapnode = CONTAINER_OF(node, IdentifierMapNode, hash);
    safe_free(mapnode->new_name);
    safe_free(node);
}

//
// Initialize map tables.
//
static void restab_init(ResolveState *state)
{
    state->vartab = hashtab_alloc(restab_alloc_mapnode, restab_free_mapnode);
    state->structtab = hashtab_alloc(restab_alloc_mapnode, restab_free_mapnode);
}

//
// Create a new state for a new scope. All of the names declared
// outside this scope will be marked as such.
//
static void restab_new_scope(ResolveState *curr, ResolveState *next)
{
    restab_init(next);

    HashIterator iter;
    for (HashNode *node = hashtab_first(curr->vartab, &iter); node; node = hashtab_next(&iter)) {
        HashNode *newnode = hashtab_lookup(next->vartab, node->key);

        IdentifierMapNode *currmap = CONTAINER_OF(node, IdentifierMapNode, hash);        
        IdentifierMapNode *nextmap = CONTAINER_OF(newnode, IdentifierMapNode, hash);    

        nextmap->from_curr_scope = false;
        nextmap->new_name = safe_strdup(currmap->new_name);    
    }

    for (HashNode *node = hashtab_first(curr->structtab, &iter); node; node = hashtab_next(&iter)) {
        HashNode *newnode = hashtab_lookup(next->structtab, node->key);

        IdentifierMapNode *currmap = CONTAINER_OF(node, IdentifierMapNode, hash);        
        IdentifierMapNode *nextmap = CONTAINER_OF(newnode, IdentifierMapNode, hash);    

        nextmap->from_curr_scope = false; 
        nextmap->new_name = safe_strdup(currmap->new_name);    
    }
}

//
// Look up a variable.
//
static IdentifierMapNode *restab_get_var(ResolveState *state, char *name)
{
    HashNode *node = hashtab_lookup(state->vartab, name);
    return CONTAINER_OF(node, IdentifierMapNode, hash);    
}

//
// Look up a struct tag.
//
static IdentifierMapNode *restab_get_struct(ResolveState *state, char *name)
{
    HashNode *node = hashtab_lookup(state->structtab, name);
    return CONTAINER_OF(node, IdentifierMapNode, hash);    
}

//
// Free the tables.
//
static void restab_free(ResolveState *state)
{
    hashtab_free(state->vartab);
    hashtab_free(state->structtab);
}

//
// Resolve a structure type.
//
static void ast_resolve_struct_type(ResolveState *state, Type *type, FileLine loc)
{
    ICE_ASSERT(type->tag == TT_STRUCT);

    IdentifierMapNode *mapnode = restab_get_struct(state, type->strct.tag);

    if (mapnode->new_name) {
        safe_free(type->strct.tag);
        type->strct.tag = safe_strdup(mapnode->new_name);
    } else {
        err_report(EC_ERROR, &loc, "use of undeclared struct `%s`.", type->strct.tag);        
        mapnode->new_name = tmp_name("error");
    }
}

//
// Resolve a function type.
//
static void ast_resolve_func_type(ResolveState *state, TypeFunction *func, FileLine loc)
{
    ast_resolve_type(state, func->ret, loc);

    for (ListNode *curr = func->parms.head; curr; curr = curr->next) {
        TypeFuncParam *param = CONTAINER_OF(curr, TypeFuncParam, list);
        ast_resolve_type(state, param->parmtype, loc);
    }
}

//
// Resolve a type.
//
static void ast_resolve_type(ResolveState *state, Type *type, FileLine loc)
{
    switch (type->tag) {
        case TT_CHAR:
        case TT_SCHAR:
        case TT_UCHAR:
        case TT_INT:
        case TT_LONG:
        case TT_UINT:
        case TT_ULONG:
        case TT_DOUBLE:
        case TT_VOID:
            break;

        case TT_FUNC:       ast_resolve_func_type(state, &type->func, loc); break;
        case TT_POINTER:    ast_resolve_type(state, type->ptr.ref, loc); break;
        case TT_ARRAY:      ast_resolve_type(state, type->array.element, loc); break;
        case TT_STRUCT:     ast_resolve_struct_type(state, type, loc); break;
    }
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
// Resolve a single variable, by name, in the current scope. If the name is
// already declared, report an error. In all cases, return the mapped, unique
// name for the variable as an allocated string.
//
static char *ast_resolve_name(ResolveState *state, char *name, FileLine loc)
{
    IdentifierMapNode *mapnode = restab_get_var(state, name);
    if (mapnode->new_name && mapnode->from_curr_scope) {
        err_report(EC_ERROR, &loc, "variable `%s` has already been declared.", name);
    } else {
        safe_free(mapnode->new_name);
        mapnode->new_name = ast_unique_varname(name);
        mapnode->from_curr_scope = true;
    }

    return safe_strdup(mapnode->new_name);
}

//
// Update a declaration entry in the given map node.
//
static void update_map_node(IdentifierMapNode *mapnode, char *name, bool from_curr_scope, bool has_linkage)
{
    safe_free(mapnode->new_name);
    mapnode->new_name = safe_strdup(name);
    mapnode->from_curr_scope = from_curr_scope;
    mapnode->has_linkage = has_linkage;
}

//
// Resolve expressions in a nested variable initializer.
//
static void ast_resolve_init(ResolveState *state, Initializer *init)
{
    if (init->tag == INIT_SINGLE) {
        ast_resolve_expression(state, init->single);
    } else {
        for (ListNode *curr = init->compound.head; curr; curr = curr->next) {
            Initializer *sub = CONTAINER_OF(curr, Initializer, list);
            ast_resolve_init(state, sub);
        }
    }
}

//
// Resolve a global variable declaration.
//
static void ast_resolve_global_var_decl(ResolveState *state, Declaration *decl)
{
    ICE_ASSERT(decl->tag == DECL_VARIABLE);
    DeclVariable *var = &decl->var;

    ast_resolve_type(state, var->type, decl->loc);

    IdentifierMapNode *mapnode = restab_get_var(state, var->name);
    update_map_node(mapnode, var->name, true, true);

    if (var->init) {
        ast_resolve_init(state, var->init);
    }
}

//
// Resolve a variable declaration.
//
static void ast_resolve_var_decl(ResolveState *state, Declaration *decl)
{
    ICE_ASSERT(decl->tag == DECL_VARIABLE);
    DeclVariable *var = &decl->var;

    ast_resolve_type(state, var->type, decl->loc);

    IdentifierMapNode *mapnode = restab_get_var(state, var->name);

    if (mapnode->new_name) {
        if (mapnode->from_curr_scope) {
            if (!(mapnode->has_linkage && var->storage_class == SC_EXTERN)) {
                err_report(EC_ERROR, &decl->loc, "conflicting declarations for variable `%s`.", var->name);
            }
        }
    }

    if (var->storage_class == SC_EXTERN) {
        update_map_node(mapnode, var->name, true, true);
    } else {
        char *new_name = ast_unique_varname(var->name);
        decl->var.name = safe_strdup(new_name);
        update_map_node(mapnode, new_name, true, false);
        safe_free(new_name);
    }

    if (var->init) {
        ast_resolve_init(state, var->init);
    }
}

//
// Check a declaration to see if duplicates another declaration in this scope;
// if so, report an error. Otherwise, generate a new unique name for the 
// declaration.
//
static void ast_resolve_declaration(ResolveState *state, Declaration *decl)
{
    switch (decl->tag) {
        case DECL_FUNCTION: ast_resolve_function(state, decl, false); break;
        case DECL_VARIABLE: ast_resolve_var_decl(state, decl); break;
        case DECL_STRUCT:   ast_resolve_struct(state, decl); break;
    }
}

//
// Resolve a variable reference.
//
static void ast_resolve_var_reference(ResolveState *state, Expression *exp)
{
    ICE_ASSERT(exp->tag == EXP_VAR);
    ExpVar *var = &exp->var;

    IdentifierMapNode *mapnode = restab_get_var(state, var->name);
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
    ast_resolve_expression(state, assign->left);
    ast_resolve_expression(state, assign->right);
}

//
// Resolve variables in a function call argument list.
//
static void ast_resolve_function_call(ResolveState *state, Expression *callexp)
{
    ICE_ASSERT(callexp->tag == EXP_FUNCTION_CALL);
    ExpFunctionCall *call = &callexp->call;

    IdentifierMapNode *mapnode = restab_get_var(state, call->name);
    if (mapnode->new_name == NULL) {
        err_report(EC_ERROR, &callexp->loc, "function `%s` has not been declared.", call->name);
    } else {
        safe_free(call->name);
        call->name = safe_strdup(mapnode->new_name);
    }

    for (ListNode *curr = call->args.head; curr; curr = curr->next) {
        Expression *arg = CONTAINER_OF(curr, Expression, list);
        ast_resolve_expression(state, arg);
    }
}

//
// Resolve a cast expression.
//
static void ast_resolve_cast_exp(ResolveState *state, ExpCast *cast, FileLine loc)
{
    ast_resolve_type(state, cast->type, loc);
    ast_resolve_expression(state, cast->exp);
}

//
// Resolve a deref expression.
//
static void ast_resolve_deref_exp(ResolveState *state, ExpDeref *deref)
{
    ast_resolve_expression(state, deref->exp);
}

//
// Resolve a addrof expression.
//
static void ast_resolve_addrof_exp(ResolveState *state, ExpAddrOf *addrof)
{
    ast_resolve_expression(state, addrof->exp);
}

//
// Resolve a subscript expression.
//
static void ast_resolve_subscript_exp(ResolveState *state, ExpSubscript *subs)
{
    ast_resolve_expression(state, subs->left);
    ast_resolve_expression(state, subs->right);
}

//
// Resolve a sizeof expression.
//
static void ast_resolve_sizeof_exp(ResolveState *state, ExpSizeof *szof, FileLine loc)
{
    switch (szof->tag) {
        case SIZEOF_EXP:    ast_resolve_expression(state, szof->exp); break;
        case SIZEOF_TYPE:   ast_resolve_type(state, szof->type, loc); break;
    }
}

//
// Resolve a dot expression.
//
static void ast_resolve_dot(ResolveState *state, ExpDot *dot)
{
    ast_resolve_expression(state, dot->exp);
}

//
// Resolve an arrow expression.
//
static void ast_resolve_arrow(ResolveState *state, ExpArrow *arrow)
{
    ast_resolve_expression(state, arrow->exp);
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
        case EXP_FUNCTION_CALL: ast_resolve_function_call(state, exp); break;
        case EXP_CAST:          ast_resolve_cast_exp(state, &exp->cast, exp->loc); break;
        case EXP_DEREF:         ast_resolve_deref_exp(state, &exp->deref); break;
        case EXP_ADDROF:        ast_resolve_addrof_exp(state, &exp->addrof); break;
        case EXP_SUBSCRIPT:     ast_resolve_subscript_exp(state, &exp->subscript); break;
        case EXP_SIZEOF:        ast_resolve_sizeof_exp(state, &exp->sizeof_, exp->loc); break;
        case EXP_DOT:           ast_resolve_dot(state, &exp->dot); break;
        case EXP_ARROW:         ast_resolve_arrow(state, &exp->arrow); break;

        case EXP_SCHAR:         break;
        case EXP_UCHAR:         break;
        case EXP_INT:           break;
        case EXP_LONG:          break;
        case EXP_UINT:          break;
        case EXP_ULONG:         break;
        case EXP_FLOAT:         break;
        case EXP_STRING:        break;
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
// Resolve variables in a for loop.
//
static void ast_resolve_for(ResolveState *state, StmtFor *for_)
{
    ResolveState newstate;

    //
    // Declarations in the init portion of the loop are in a new scope 
    //
    restab_new_scope(state, &newstate);

    switch (for_->init->tag) {
        case FI_DECLARATION:    ast_resolve_declaration(&newstate, for_->init->decl); break;
        case FI_EXPRESSION:     ast_resolve_expression(&newstate, for_->init->exp); break;
        case FI_NONE:           break;
    }

    if (for_->cond) {
        ast_resolve_expression(&newstate, for_->cond);
    }

    if (for_->post) {
        ast_resolve_expression(&newstate, for_->post);
    }

    ast_resolve_statement(&newstate, for_->body);

    restab_free(&newstate);
}

//
// Resolve variables in a while loop.
//
static void ast_resolve_while(ResolveState *state, StmtWhile *while_)
{
    ast_resolve_expression(state, while_->cond);
    ast_resolve_statement(state, while_->body);
}

//
// Resolve variables in a do while loop.
//
static void ast_resolve_do_while(ResolveState *state, StmtDoWhile *dowhile)
{
    ast_resolve_expression(state, dowhile->cond);
    ast_resolve_statement(state, dowhile->body);
}

//
// Resolve variables in a switch statement.
//
static void ast_resolve_switch(ResolveState *state, StmtSwitch *switch_)
{
    ast_resolve_expression(state, switch_->cond);
    ast_resolve_statement(state, switch_->body);
}

//
// Resolve variables in a case statement.
//
static void ast_resolve_case(ResolveState *state, StmtCase *case_)
{
    ast_resolve_statement(state, case_->stmt);
}

//
// Resolve variables in a default statement.
//
static void ast_resolve_default(ResolveState *state, StmtDefault *def)
{
    ast_resolve_statement(state, def->stmt);
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
        case STMT_FOR:          ast_resolve_for(state, &stmt->for_); break;
        case STMT_WHILE:        ast_resolve_while(state, &stmt->while_); break;
        case STMT_DOWHILE:      ast_resolve_do_while(state, &stmt->dowhile); break;
        case STMT_BREAK:        break;
        case STMT_CONTINUE:     break;
        case STMT_SWITCH:       ast_resolve_switch(state, &stmt->switch_); break;
        case STMT_CASE:         ast_resolve_case(state, &stmt->case_); break;
        case STMT_DEFAULT:      ast_resolve_default(state, &stmt->default_); break;
    }
}

//
// Resolve variables for one function.
//
static void ast_resolve_function(ResolveState *state, Declaration *decl, bool global)
{
    ICE_ASSERT(decl->tag == DECL_FUNCTION);

    DeclFunction *func = &decl->func;

    if (!global && func->storage_class == SC_STATIC) {
        err_report(EC_ERROR, &decl->loc, "invalid storage class for function `%s`.", func->name);
    }
    
    IdentifierMapNode *mapnode = restab_get_var(state, func->name);
    if (mapnode->new_name) {
        if (mapnode->from_curr_scope && !mapnode->has_linkage) {
            err_report(EC_ERROR, &decl->loc, "duplicate declaration for function `%s`.\n", func->name);
        } 
    } 

    safe_free(mapnode->new_name);
    mapnode->new_name = safe_strdup(func->name);
    mapnode->from_curr_scope = true;
    mapnode->has_linkage = true;

    ResolveState newstate;
    restab_new_scope(state, &newstate);

    //
    // Function parameters are in their own scope.
    //
    for (ListNode *curr = func->parms.head; curr; curr = curr->next) {
        FuncParameter *param = CONTAINER_OF(curr, FuncParameter, list);
        char *new_name = ast_resolve_name(&newstate, param->name, decl->loc);

        safe_free(param->name);
        param->name = new_name;
    }

    //
    // Resolve parameter types.
    //
    for (ListNode *curr = func->type->func.parms.head; curr; curr= curr->next) {
        TypeFuncParam *tfp = CONTAINER_OF(curr, TypeFuncParam, list);
        ast_resolve_type(state, tfp->parmtype, decl->loc);
    }

    if (func->type->func.ret) {
        ast_resolve_type(state, func->type->func.ret, decl->loc);
    }

    //
    // New scope for function body.
    //
    ast_resolve_block(&newstate, func->body);

    if (func->body.head && !global) {
        err_report(EC_ERROR, &decl->loc, "function `%s` may not be declared in another function.", func->name);
    } 

    restab_free(&newstate);
}

//
// Resolve a structure declaration.
//
static void ast_resolve_struct(ResolveState *state, Declaration *decl)
{
    IdentifierMapNode *node = restab_get_struct(state, decl->strct.tag);
    
    char *unique_tag;

    if (node->new_name == NULL || !node->from_curr_scope) {
        unique_tag = ast_unique_varname(decl->strct.tag);
        update_map_node(node, unique_tag, true, false);
    } else {
        unique_tag = node->new_name;
    }

    safe_free(decl->strct.tag);
    decl->strct.tag = safe_strdup(unique_tag);

    for (ListNode *curr = decl->strct.memb.head; curr; curr = curr->next) {
        DeclStructMember *memb = CONTAINER_OF(curr, DeclStructMember, list);
        ast_resolve_type(state, memb->type, decl->loc);
    }
}

//
// Walk an AST and
// - ensure that variable usage and declarations are semantically ok
//   - variables are declared before use
//   - variables are not declared more than once in the same scope
// - rename variables to unique names within the entire program
//
void ast_resolve(AstProgram *prog)
{
    ResolveState state;
    restab_init(&state);

    for (ListNode *curr = prog->decls.head; curr; curr = curr->next) {
        Declaration *decl = CONTAINER_OF(curr, Declaration, list);
        switch (decl->tag) {
            case DECL_FUNCTION: ast_resolve_function(&state, decl, true); break;
            case DECL_VARIABLE: ast_resolve_global_var_decl(&state, decl); break;
            case DECL_STRUCT:   ast_resolve_struct(&state, decl); break;
        }
    } 

    restab_free(&state);
}
