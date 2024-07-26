#include "looplabel.h"

#include "ast.h"
#include "errors.h"
#include "ice.h"
#include "list.h"

typedef struct {
    int break_label;
    int cont_label;
    int switch_label;
} LabelState;

static void ast_label_statement(LabelState state, Statement *stmt);
static void ast_label_block(LabelState state, List items);

//
// Allocate a new, program wide unique, integer loop label.
//
static int ast_alloc_loop_label(void)
{
    static int next_label = 0;
    return next_label++;
}

//
// Label loops inside an if statement.
//
static void ast_label_if(LabelState state, StmtIf *if_)
{
    ast_label_statement(state, if_->thenpart);
    if (if_->elsepart) {
        ast_label_statement(state, if_->elsepart);
    }
}

//
// Label loops inside a label statement.
//
static void ast_label_label(LabelState state, StmtLabel *stmt)
{
    ast_label_statement(state, stmt->stmt);
}

//
// Label loops inside a compound statement.
//
static void ast_label_compound(LabelState state, StmtCompound *compound)
{
    ast_label_block(state, compound->items);
}

//
// Label a while loop.
//
static void ast_label_while(LabelState state, StmtWhile *while_)
{    
    while_->label = ast_alloc_loop_label();

    LabelState newstate;
    
    newstate.break_label = while_->label;
    newstate.cont_label = while_->label;
    newstate.switch_label = state.switch_label;

    ast_label_statement(newstate, while_->body);
}

//
// Label a for loop.
//
static void ast_label_for(LabelState state, StmtFor *for_)
{
    for_->label = ast_alloc_loop_label();

    LabelState newstate;
    newstate.break_label = for_->label;
    newstate.cont_label = for_->label;
    newstate.switch_label = state.switch_label;

    ast_label_statement(newstate, for_->body);
}

//
// Label a do while loop.
//
static void ast_label_do_while(LabelState state, StmtDoWhile *dowhile)
{
    dowhile->label = ast_alloc_loop_label();

    LabelState newstate;
    newstate.break_label = dowhile->label;
    newstate.cont_label = dowhile->label;
    newstate.switch_label = state.switch_label;

    ast_label_statement(newstate, dowhile->body);
}

//
// Label a break statement.
//
static void ast_label_break(LabelState state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_BREAK);

    if (state.break_label == -1) {
        err_report(EC_ERROR, &stmt->loc, "`break` statment with no enclosing loop.\n");
    } else {
        stmt->break_.label = state.break_label;
    }
}

//
// Label a continue statement.
//
static void ast_label_continue(LabelState state, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_CONTINUE);

    if (state.cont_label == -1) {
        err_report(EC_ERROR, &stmt->loc, "`continue` statment with no enclosing loop.\n");
    } else {
        stmt->continue_.label = state.cont_label;
    }
}

//
// Label a switch statement.
// 
static void ast_label_switch(LabelState state, StmtSwitch *switch_)
{
    //
    // Unlike loops, switch switch statements only honor enclosed break
    // statements. continue statements still go to the next closing loop,
    // if there is one.
    //
    switch_->label = ast_alloc_loop_label();
    state.break_label = switch_->label;
    state.switch_label = switch_->label;
    ast_label_statement(state, switch_->body);
}

//
// Label a case statement.
//
static void ast_label_case(LabelState state, StmtCase *case_)
{    
    case_->label = state.switch_label;
    ast_label_statement(state, case_->stmt);
}

//
// Label a default statement.
//
static void ast_label_default(LabelState state, StmtDefault *def)
{
    def->label = state.switch_label;
    ast_label_statement(state, def->stmt);
}

//
// Label loops inside a statement.
//
static void ast_label_statement(LabelState state, Statement *stmt)
{
    switch (stmt->tag) {
        case STMT_NULL:         break;
        case STMT_RETURN:       break;
        case STMT_IF:           ast_label_if(state, &stmt->ifelse); break;
        case STMT_EXPRESSION:   break;
        case STMT_LABEL:        ast_label_label(state, &stmt->label); break;
        case STMT_GOTO:         break;
        case STMT_COMPOUND:     ast_label_compound(state, &stmt->compound); break;
        case STMT_WHILE:        ast_label_while(state, &stmt->while_); break;
        case STMT_FOR:          ast_label_for(state, &stmt->for_); break;
        case STMT_DOWHILE:      ast_label_do_while(state, &stmt->dowhile); break;
        case STMT_BREAK:        ast_label_break(state, stmt); break;
        case STMT_CONTINUE:     ast_label_continue(state, stmt); break;
        case STMT_SWITCH:       ast_label_switch(state, &stmt->switch_); break;
        case STMT_CASE:         ast_label_case(state, &stmt->case_); break;
        case STMT_DEFAULT:      ast_label_default(state, &stmt->default_); break;
    }
}

//
// Label loops inside a block. labels are the label of the
// current innermost loop or -1 if there is no containing
// loop. 
//
static void ast_label_block(LabelState state, List items)
{
    for (ListNode *curr = items.head; curr; curr = curr->next) {
        BlockItem *blki = CONTAINER_OF(curr, BlockItem, list);

        switch (blki->tag) {
            case BI_STATEMENT:      ast_label_statement(state, blki->stmt); break;
            case BI_DECLARATION:    break;
        }
    }
}

//
// Label loops inside a function.
//
static void ast_label_function(DeclFunction *func)
{
    LabelState state = { -1, -1, -1 };
    ast_label_block(state, func->body);
}

//
// Walk the AST and label loops. Assign each loop a unique label.
// Assign each break or continue statement the label of the 
// innermost surrounding loop.
//
void ast_label_loops(AstProgram *prog)
{
    for (ListNode *curr = prog->decls.head; curr; curr = curr->next) {
        Declaration *decl = CONTAINER_OF(curr, Declaration, list);
        switch (decl->tag) {
            case DECL_FUNCTION: ast_label_function(&decl->func); break;
            case DECL_VARIABLE: break;
        }
    }
}
