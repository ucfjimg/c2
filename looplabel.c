#include "looplabel.h"

#include "ast.h"
#include "errors.h"
#include "ice.h"
#include "list.h"

static void ast_label_statement(int label, Statement *stmt);
static void ast_label_block(int label, List items);

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
static void ast_label_if(int label, StmtIf *if_)
{
    ast_label_statement(label, if_->thenpart);
    if (if_->elsepart) {
        ast_label_statement(label, if_->elsepart);
    }
}

//
// Label loops inside a label statement.
//
static void ast_label_label(int label, StmtLabel *stmt)
{
    ast_label_statement(label, stmt->stmt);
}

//
// Label loops inside a compound statement.
//
static void ast_label_compound(int label, StmtCompound *compound)
{
    ast_label_block(label, compound->items);
}

//
// Label a while loop.
//
static void ast_label_while(int label, StmtWhile *while_)
{
    while_->label = ast_alloc_loop_label();
    ast_label_statement(while_->label, while_->body);
}

//
// Label a for loop.
//
static void ast_label_for(int label, StmtFor *for_)
{
    for_->label = ast_alloc_loop_label();
    ast_label_statement(for_->label, for_->body);
}

//
// Label a do while loop.
//
static void ast_label_do_while(int label, StmtDoWhile *dowhile)
{
    dowhile->label = ast_alloc_loop_label();
    ast_label_statement(dowhile->label, dowhile->body);
}

//
// Label a break statement.
//
static void ast_label_break(int label, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_BREAK);

    if (label == -1) {
        err_report(EC_ERROR, &stmt->loc, "`break` statment with no enclosing loop.\n");
    } else {
        stmt->break_.label = label;
    }
}

//
// Label a continue statement.
//
static void ast_label_continue(int label, Statement *stmt)
{
    ICE_ASSERT(stmt->tag == STMT_CONTINUE);

    if (label == -1) {
        err_report(EC_ERROR, &stmt->loc, "`continue` statment with no enclosing loop.\n");
    } else {
        stmt->continue_.label = label;
    }
}

//
// Label loops inside a statement.
//
static void ast_label_statement(int label, Statement *stmt)
{
    switch (stmt->tag) {
        case STMT_NULL:         break;
        case STMT_RETURN:       break;
        case STMT_IF:           ast_label_if(label, &stmt->ifelse); break;
        case STMT_EXPRESSION:   break;
        case STMT_LABEL:        ast_label_label(label, &stmt->label); break;
        case STMT_GOTO:         break;
        case STMT_COMPOUND:     ast_label_compound(label, &stmt->compound); break;
        case STMT_WHILE:        ast_label_while(label, &stmt->while_); break;
        case STMT_FOR:          ast_label_for(label, &stmt->for_); break;
        case STMT_DOWHILE:      ast_label_do_while(label, &stmt->dowhile); break;
        case STMT_BREAK:        ast_label_break(label, stmt); break;
        case STMT_CONTINUE:     ast_label_continue(label, stmt); break;
    }
}

//
// Label loops inside a block. `label` is the label of the
// current innermost loop or -1 if there is no containing
// loop.
//
static void ast_label_block(int label, List items)
{
    for (ListNode *curr = items.head; curr; curr = curr->next) {
        BlockItem *blki = CONTAINER_OF(curr, BlockItem, list);

        switch (blki->tag) {
            case BI_STATEMENT:      ast_label_statement(label, blki->stmt); break;
            case BI_DECLARATION:    break;
        }
    }
}


//
// Label loops inside a function.
//
static void ast_label_function(AstFunction *func)
{
    ast_label_block(-1, func->stmts);
}

//
// Walk the AST and label loops. Assign each loop a unique label.
// Assign each break or continue statement the label of the 
// innermost surrounding loop.
//
void ast_label_loops(AstNode *ast)
{
    ICE_ASSERT(ast->tag == AST_PROGRAM);

    ast_label_function(&ast->prog.func->func);
}
