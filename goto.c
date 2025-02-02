#include "goto.h"

#include "ast.h"
#include "errors.h"
#include "hashtab.h"
#include "ice.h"
#include "list.h"
#include "safemem.h"
#include "temporary.h"

typedef struct {
    HashNode hash;
    bool is_defined;                // true if defined (else just referenced)
    bool is_used;                   // true if label has been used
    FileLine loc;                   // location of first reference by goto
    char *new_name;                 // unique generated name
} LabelNode;

typedef struct {
    HashTable *hashtab;
} GotoState;

static void ast_validate_statement(GotoState *state, Statement *stmt);

//
// Allocate a hash node for the label table.
//
static HashNode *goto_alloc_hashnode(void)
{
    return safe_zalloc(sizeof(LabelNode));
}

//
// Free a hash node from the label table.
//
static void goto_free_hashnode(HashNode *node)
{
    LabelNode *lnode = CONTAINER_OF(node, LabelNode, hash);
    safe_free(lnode->new_name);
    safe_free(lnode);
}

//
// Allocate a state for analyzing one function's gotos and labels.
//
static GotoState *goto_state_alloc(void)
{
    GotoState *state = safe_zalloc(sizeof(GotoState));
    state->hashtab = hashtab_alloc(goto_alloc_hashnode, goto_free_hashnode);
    return state;
}

//
// Free state for analyzing one function.
//
static void goto_state_free(GotoState *state)
{
    hashtab_free(state->hashtab);
    safe_free(state);
}

//
// Get a label from the label table.
//
static LabelNode *goto_get_label(GotoState *state, char *label)
{
    HashNode *node = hashtab_lookup(state->hashtab, label);
    return CONTAINER_OF(node, LabelNode, hash);
}

//
// Generate a unique label.
//
static char *make_unique_label(char *label)
{
    return tmp_name(label); 
}

//
// Validate one label.
//
static void ast_validate_label(GotoState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_LABEL);
    StmtLabel *label = &stmt->label;

    LabelNode *node = goto_get_label(state, label->name);

    if (node->is_defined) {
        err_report(EC_ERROR, &stmt->loc, "label `%s` has multiple definitions.", label->name); 
    }

    node->is_defined = true;
    if (node->new_name == NULL) {
        node->new_name = make_unique_label(label->name);
    }

    safe_free(label->name);
    label->name = safe_strdup(node->new_name);

    ast_validate_statement(state, label->stmt);
}

//
// Validate a goto.
//
static void ast_validate_goto_stmt(GotoState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_GOTO);
    StmtGoto *goto_ = &stmt->goto_;

    LabelNode *node = goto_get_label(state, goto_->target);
    
    if (node->new_name == NULL) {
        node->new_name = make_unique_label(goto_->target);
    }

    safe_free(goto_->target);
    goto_->target = safe_strdup(node->new_name);

    if (!node->is_used) {
        node->is_used = true;
        node->loc = stmt->loc;
    }
}

//
// Validate an if statement.
//
static void ast_validate_if(GotoState *state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_IF);
    
    ast_validate_statement(state, stmt->ifelse.thenpart);
    if (stmt->ifelse.elsepart) {
        ast_validate_statement(state, stmt->ifelse.elsepart);
    }
}

//
// Validate a block.
//
static void ast_validate_block(GotoState *state, List items)
{
    for (ListNode *curr = items.head; curr; curr = curr->next) {
        BlockItem *blki = CONTAINER_OF(curr, BlockItem, list);
        if (blki->tag == BI_STATEMENT) {
            ast_validate_statement(state, blki->stmt);
        }
    }
}

//
// Validate a compound statement.
//
static void ast_validate_compound(GotoState *state, StmtCompound *compound)
{
    ast_validate_block(state, compound->items);
}

//
// Validate a for loop.
//
static void ast_validate_for(GotoState *state, StmtFor *for_)
{
    ast_validate_statement(state, for_->body);
}

//
// Validate a while loop.
//
static void ast_validate_while(GotoState *state, StmtWhile *while_)
{
    ast_validate_statement(state, while_->body);
}

//
// Validate a do while loop.
//
static void ast_validate_do_while(GotoState *state, StmtDoWhile *dowhile)
{
    ast_validate_statement(state, dowhile->body);
}

//
// Validate a switch statement.
//
static void ast_validate_switch(GotoState *state, StmtSwitch *switch_)
{
    ast_validate_statement(state, switch_->body);
}

//
// Validate a case statement.
//
static void ast_validate_case(GotoState *state, StmtCase *case_)
{
    ast_validate_statement(state, case_->stmt);
}

//
// Validate a default statement.
//
static void ast_validate_default(GotoState *state, StmtDefault *def)
{
    ast_validate_statement(state, def->stmt);
}

//
// Validate one statement.
//
static void ast_validate_statement(GotoState *state, Statement *stmt)
{
    switch (stmt->tag) {
        case STMT_LABEL:    ast_validate_label(state, stmt); break;
        case STMT_GOTO:     ast_validate_goto_stmt(state, stmt); break;

        case STMT_IF:       ast_validate_if(state, stmt); break;
        case STMT_COMPOUND: ast_validate_compound(state, &stmt->compound); break;
        case STMT_FOR:      ast_validate_for(state, &stmt->for_); break;
        case STMT_WHILE:    ast_validate_while(state, &stmt->while_); break;
        case STMT_DOWHILE:  ast_validate_do_while(state, &stmt->dowhile); break;
        case STMT_SWITCH:   ast_validate_switch(state, &stmt->switch_); break;
        case STMT_CASE:     ast_validate_case(state, &stmt->case_); break;
        case STMT_DEFAULT:  ast_validate_default(state, &stmt->default_); break;

        case STMT_NULL:
        case STMT_RETURN:
        case STMT_EXPRESSION:
        case STMT_BREAK:
        case STMT_CONTINUE:
            break;
    } 
}

//
// Validate all gotos and labels in a function.
//
static void ast_goto_function(DeclFunction *func)
{
    GotoState *state = goto_state_alloc();

    ast_validate_block(state, func->body);

    //
    // Check to see if there are any undefined entries in the hash table.
    //
    HashIterator iter;
    for (HashNode *curr = hashtab_first(state->hashtab, &iter); curr; curr = hashtab_next(&iter)) {
        LabelNode *node = CONTAINER_OF(curr, LabelNode, hash);

        if (!node->is_defined) {
            err_report(EC_ERROR, &node->loc, "label `%s` used but never defined.\n", curr->key);
        }        
    }

    goto_state_free(state);
}

//
// Validate a program for proper use of goto. 
// - labels must not be multiply defined in the same function.
// - labels referenced by goto must exist in the same function.
//
void ast_validate_goto(AstProgram *prog)
{
    for (ListNode *curr = prog->decls.head; curr; curr = curr->next) {
        Declaration *decl = CONTAINER_OF(curr, Declaration, list);

        switch (decl->tag) {
            case DECL_FUNCTION: ast_goto_function(&decl->func); break;
            case DECL_VARIABLE: break;
            case DECL_STRUCT:   break;
        }
    }
}
