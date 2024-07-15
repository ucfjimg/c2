#include "codegen.h"

#include "asm-ast.h"
#include "list.h"

//
// Generate code for an expression.
//
static AsmOperand *cg_operand(Expression *exp)
{
    List instrs;

    switch (exp->tag) {
        case EXP_INT:   return aoper_imm(exp->intval);
    }

    return NULL;
}

//
// Generate code for a return statement.
//
static AsmNode *cg_return_stmt(StmtReturn *ret)
{
    List instrs;

    list_clear(&instrs);

    AsmNode *instr = asm_mov(cg_operand(ret->exp), aoper_reg(REG_RAX));
    list_push_back(&instrs, &instr->list);

    instr = asm_ret();
    list_push_back(&instrs, &instr->list);

    return CONTAINER_OF(instrs.head, AsmNode, list);
}

//
// Generate code for a statement.
//
static AsmNode *cg_statement(Statement *stmt)
{
    switch (stmt->tag) {
        case STMT_NULL:     break;
        case STMT_RETURN:   return cg_return_stmt(&stmt->ret);
    }

    return NULL;
}

//
// Generate code for a function.
//
static AsmNode *cg_function(AstFunction *func)
{
    AsmNode *asmfunc = asm_func(func->name);
    list_append(&asmfunc->func.body, &cg_statement(func->stmt)->list);

    return asmfunc;
}

//
// Generate code for the entire program.
//
static AsmNode *cg_program(AstProgram *prog)
{
    AsmNode *func = codegen(prog->func);

    AsmNode *asmprog = asm_prog();  
    asmprog->prog.func = func;

    return asmprog;
}

//
// Generate code from the AST.
//
AsmNode *codegen(AstNode *ast)
{
    switch (ast->tag) {
        case AST_PROGRAM:  return cg_program(&ast->prog);
        case AST_FUNCTION: return cg_function(&ast->func);
    }

    return NULL;
}
