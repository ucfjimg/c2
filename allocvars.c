#include "allocvars.h"

#include "hashtab.h"
#include "ice.h"
#include "list.h"
#include "safemem.h"

#include <stdbool.h>

typedef struct
{
    HashTable *vars;
    int next_offset;
} VarTable;

typedef struct 
{
    HashNode hash;
    bool allocated;
    int offset;
} VarTableNode;

//
// Allocate a hash node for the variable table.
//
static HashNode *vartab_alloc_varnode(void)
{
    VarTableNode *node = safe_zalloc(sizeof(VarTableNode));
    return &node->hash;
}

//
// Free a hash node from the variable table.
//
static void vartab_free_varnode(HashNode *node)
{
    safe_free(node);
}

//
// Initialize variable table.
//
static void vartab_init(VarTable *table)
{
    table->vars = hashtab_alloc(vartab_alloc_varnode, vartab_free_varnode);
    table->next_offset = 0;
}

//
// Look up a local variable.
//
static VarTableNode *vartab_get(VarTable *table, char *name)
{
    HashNode *node = hashtab_lookup(table->vars, name);
    return CONTAINER_OF(node, VarTableNode, hash);    
}

//
// Free the variable table
//
static void vartab_free(VarTable *table)
{
    hashtab_free(table->vars);
}

//
// Apply pseudoregister replacement to an operand.
// The operand pointer may be free'd and reallocated.
//
static void asm_alloc_operand(VarTable *vartab, AsmOperand **oper)
{
    if ((*oper)->tag == AOP_PSEUDOREG) {
        VarTableNode *var = vartab_get(vartab, (*oper)->pseudoreg);
        if (!var->allocated) {
            //
            // TODO 4 because right now, all variables are integers.
            //
            vartab->next_offset -= 4;
            var->allocated = true;
            var->offset = vartab->next_offset;
        }

        //
        // Replace with offset from stack frame.
        //
        aoper_free(*oper);
        *oper = aoper_stack(var->offset);
    }
}

//
// Apply pseudoregister replacement to a MOV instruction.
//
static void asm_alloc_mov(VarTable *vartab, AsmNode *instr)
{
    ICE_ASSERT(instr->tag == ASM_MOV);

    asm_alloc_operand(vartab, &instr->mov.src);
    asm_alloc_operand(vartab, &instr->mov.dst);
}

//
// Apply pseudoregister replacement to a unary instruction.
//
static void asm_alloc_unary(VarTable *vartab, AsmNode *instr)
{
    ICE_ASSERT(instr->tag == ASM_UNARY);

    asm_alloc_operand(vartab, &instr->unary.arg);
}

//
// Apply pseudoregister replacement to a binary instruction.
//
static void asm_alloc_binary(VarTable *vartab, AsmNode *instr)
{
    ICE_ASSERT(instr->tag == ASM_BINARY);

    asm_alloc_operand(vartab, &instr->binary.src);
    asm_alloc_operand(vartab, &instr->binary.dst);
}

//
// Apply pseudoregister replacement to an idiv instruction.
//
static void asm_alloc_idiv(VarTable *vartab, AsmNode *instr)
{
    ICE_ASSERT(instr->tag == ASM_IDIV);

    asm_alloc_operand(vartab, &instr->idiv.arg);
}

//
// Apply pseudoregister replacement to an instruction.
//
static void asm_alloc_instr(VarTable *vartab, AsmNode *instr)
{
    switch (instr->tag) {
        case ASM_MOV:       asm_alloc_mov(vartab, instr); break;
        case ASM_RET:       break;
        case ASM_CDQ:       break;
        case ASM_UNARY:     asm_alloc_unary(vartab, instr); break;
        case ASM_BINARY:    asm_alloc_binary(vartab, instr); break;
        case ASM_IDIV:      asm_alloc_idiv(vartab, instr); break;

        default:
            ICE_ASSERT(((void)"invalid asm-ast node in asm_alloc_instr", false));
    }
}

//
// Apply pseudoregister replacement to a function.
//
static void asm_alloc_func(VarTable *vartab, AsmNode *func)
{
    ICE_ASSERT(func->tag == ASM_FUNC);

    //
    // Reset local variables for start of function
    //
    vartab->next_offset = 0;

    for (ListNode *curr = func->func.body.head; curr; curr = curr->next) {
        AsmNode *node = CONTAINER_OF(curr, AsmNode, list);
        asm_alloc_instr(vartab, node);
    }

    func->func.locals_size = -vartab->next_offset;
}

//
// Allocate pseudoregisters in a function's stack frame. 
//
void asm_allocate_vars(AsmNode *prog)
{
    ICE_ASSERT(prog->tag == ASM_PROG);

    VarTable vartab;
    vartab_init(&vartab);
    asm_alloc_func(&vartab, prog->prog.func);
    vartab_free(&vartab);
}
