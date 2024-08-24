#include "switch.h"

#include "ast.h"
#include "errors.h"
#include "ice.h"
#include "list.h"
#include "safemem.h"

#include <stdbool.h>

typedef struct {
    int label;              // loop label of innermost switch
    Type *type;             // condition type of innermost switch
    List cases;             // list of collected case values <CaseLabel>
    bool has_default;       // true if a default case has been seen
} SwitchState;

static void ast_switch_statement(SwitchState *state, Statement *stmt);
static void ast_switch_block(SwitchState *state, List items);

//
// Validate switches in an if statement.
//
static void ast_switch_if(SwitchState *state, StmtIf *ifelse)
{
    ast_switch_statement(state, ifelse->thenpart);
    if (ifelse->elsepart) {
        ast_switch_statement(state, ifelse->elsepart);
    }
}

//
// Validate switches in a label statement.
//
static void ast_switch_label(SwitchState *state, StmtLabel *label)
{
    ast_switch_statement(state, label->stmt);
}

//
// Validate switches in a compound statement.
//
static void ast_switch_compound(SwitchState *state, StmtCompound *compound)
{
    ast_switch_block(state, compound->items);
}

//
// Validate switches in a while loop.
//
static void ast_switch_while(SwitchState *state, StmtWhile *while_)
{
    ast_switch_statement(state, while_->body);
}

//
// Validate switches in a for loop.
//
static void ast_switch_for(SwitchState *state, StmtFor *for_)
{
    ast_switch_statement(state, for_->body);
}

//
// Validate switches in a do while loop.
//
static void ast_switch_do_while(SwitchState *state, StmtDoWhile *dowhile)
{
    ast_switch_statement(state, dowhile->body);
}

//
// Validate a switch statement.
//
static void ast_switch_switch(StmtSwitch *switch_)
{
    SwitchState state;

    state.label = switch_->label;
    state.type = switch_->cond->type;
    state.has_default = false;
    list_clear(&state.cases);

    ast_switch_statement(&state, switch_->body);

    switch_->cases = state.cases;
    switch_->has_default = state.has_default;
}

//
// Validate a case statement.
//
static void ast_switch_case(SwitchState *state, StmtCase *case_, FileLine loc)
{
    if (state == NULL) {
        err_report(EC_ERROR, &loc, "case with no containing switch.");
        return;
    }

    bool is_unsigned = type_unsigned(state->type);

    unsigned long value = case_->value;

    //
    // $TARGET this is assuming that int is the same on host and target
    //
    if (state->type->tag == TT_INT || state->type->tag == TT_UINT) {
        value = (unsigned)value;
    }

    //
    // Check for duplicate case labels.
    //
    for (ListNode *curr = state->cases.head; curr; curr = curr->next) {
        CaseLabel *label = CONTAINER_OF(curr, CaseLabel, list);
        if (value == label->value) {
            if (is_unsigned) {
                err_report(EC_ERROR, &loc, "duplicate case value `%ld`.", (long)value);
            } else {
                err_report(EC_ERROR, &loc, "duplicate case value `%lu`.", value);
            }
            return;
        }
    }

    //
    // Not found, add it.
    //
    CaseLabel *label = safe_zalloc(sizeof(CaseLabel));
    label->value = case_->value;
    list_push_back(&state->cases, &label->list);

    ast_switch_statement(state, case_->stmt);
}

//
// Validate a default statement.
//
static void ast_switch_default(SwitchState *state, StmtDefault *def, FileLine loc)
{
    if (state == NULL) {
        err_report(EC_ERROR, &loc, "default with no containing switch.");
        return;
    }

    if (state->has_default) {
        err_report(EC_ERROR, &loc, "duplicate default label.");
    }

    def->label = state->label;
    state->has_default = true;
    

    ast_switch_statement(state, def->stmt);
}

//
// Validate switches in a statement.
//
static void ast_switch_statement(SwitchState *state, Statement *stmt)
{
    switch (stmt->tag) {
        case STMT_NULL:         break;
        case STMT_RETURN:       break;
        case STMT_IF:           ast_switch_if(state, &stmt->ifelse); break;
        case STMT_EXPRESSION:   break;
        case STMT_LABEL:        ast_switch_label(state, &stmt->label); break;
        case STMT_GOTO:         break;
        case STMT_COMPOUND:     ast_switch_compound(state, &stmt->compound); break;
        case STMT_WHILE:        ast_switch_while(state, &stmt->while_); break;
        case STMT_FOR:          ast_switch_for(state, &stmt->for_); break;
        case STMT_DOWHILE:      ast_switch_do_while(state, &stmt->dowhile); break;
        case STMT_BREAK:        break;
        case STMT_CONTINUE:     break;
        case STMT_SWITCH:       ast_switch_switch(&stmt->switch_); break;
        case STMT_CASE:         ast_switch_case(state, &stmt->case_, stmt->loc); break;
        case STMT_DEFAULT:      ast_switch_default(state, &stmt->default_, stmt->loc); break;
    }
}

//
// Validate switches inside a block.
//
static void ast_switch_block(SwitchState *state, List items)
{
    for (ListNode *curr = items.head; curr; curr = curr->next) {
        BlockItem *blki = CONTAINER_OF(curr, BlockItem, list);
        
        switch (blki->tag) {
            case BI_STATEMENT:      ast_switch_statement(state, blki->stmt); break;
            case BI_DECLARATION:    break;
        }
    }
}

//
// Validate switches inside a function.
//
static void ast_switch_function(DeclFunction *func)
{
    //
    // Start will a NULL state since there is no enclosing switch
    // at the function level.
    //
    ast_switch_block(NULL, func->body);
}

//
// Walk through the AST and resolve case statements inside
// a switch. 
//
void ast_validate_switch(AstProgram *prog)
{
    for (ListNode *curr = prog->decls.head; curr; curr = curr->next) {
        Declaration *decl = CONTAINER_OF(curr, Declaration, list);
        switch (decl->tag) {
            case DECL_FUNCTION: ast_switch_function(&decl->func); break;
            case DECL_VARIABLE: break;
            case DECL_STRUCT:   break;
        }
    }
}
