#include "codegen.h"

#include "asm-ast.h"
#include "ice.h"
#include "list.h"
#include "safemem.h"
#include "symtab.h"
#include "temporary.h"
#include "typetab.h"

#include <ctype.h>

typedef struct
{
    List code;
    List *statics;
    BackEndSymbolTable *bstab;
    SymbolTable *stab;
    TypeTable *typetab;
    char *neg_zero;
    char *dbl_to_uint_ub;
} CodegenState;

static Register int_arg_regs[] =
    {
        REG_RDI,
        REG_RSI,
        REG_RDX,
        REG_RCX,
        REG_R8,
        REG_R9,
};
static const int int_arg_reg_count = sizeof(int_arg_regs) / sizeof(int_arg_regs[0]);

static Register float_arg_regs[] =
    {
        REG_XMM0,
        REG_XMM1,
        REG_XMM2,
        REG_XMM3,
        REG_XMM4,
        REG_XMM5,
        REG_XMM6,
        REG_XMM7,
};
static const int float_arg_reg_count = sizeof(float_arg_regs) / sizeof(float_arg_regs[0]);

#define DBL_TO_UINT_UPPER_FLT 9223372036854775808.0
#define DBL_TO_UINT_UPPER_INT 9223372036854775808lu

typedef enum {
    STC_MEMORY,
    STC_SSE,
    STC_INTEGER,
} StructClass;

typedef struct {
    size_t size;
    size_t eightbytes;
    StructClass sc[1];
} ClassifiedStruct;

typedef struct {
    ListNode list;
    Type *type;
} TypeList;

typedef struct {
    List *ints;
    int nints;
    List *floats;
    int nfloats;
    List *stack;
    int intregs;
} ClassifyParmsState;

typedef struct {
    ListNode list;
    AsmType *type;
    AsmOperand *oper;
} TypedOperand;

static bool classify_return_value(CodegenState *state, TacNode *retval, List *ints, List *doubles);

//
// Create a nested state.
//
static CodegenState nested_state(CodegenState *outer)
{
    CodegenState nested = *outer;
    return nested;
}

//
// Return true if the given TAC operand is unsigned.
//
// The TAC node must be either a constant or a variable.
//
static bool codegen_operand_unsigned(CodegenState *state, TacNode *tac)
{
    if (tac->tag == TAC_CONST)
    {
        return const_unsigned(&tac->constant);
    }

    if (tac->tag == TAC_VAR)
    {
        Symbol *sym = stab_lookup(state->stab, tac->var.name);
        return type_unsigned(sym->type);
    }

    ICE_ASSERT(((void)"invalid TAC tag in codegen_operand_unsigned", false));
    return false;
}

//
// Return true if the given TAC operand is a float.
//
// The TAC node must either be a variable or a constant.
//
static bool codegen_operand_float(CodegenState *state, TacNode *tac)
{
    //
    // If it's a constant, the type is specified.
    //
    if (tac->tag == TAC_CONST)
    {
        return tac->constant.tag == CON_FLOAT;
    }

    //
    // If it's a variable, look it up in the symbol
    // table to see what type it is.
    //
    if (tac->tag == TAC_VAR)
    {
        Symbol *sym = stab_lookup(state->stab, tac->var.name);
        return sym->type->tag == TT_DOUBLE;
    }

    ICE_ASSERT(((void)"TAC node in codegen_operand_float was not an operand.", false));
    return false;
}

//
// Return true if the given TAC operand is a pointer.
//
// The TAC node must either be a variable or a constant.
//
static bool codegen_operand_pointer(CodegenState *state, TacNode *tac)
{
    //
    // If it's a constant, it's not a pointer.
    //
    if (tac->tag == TAC_CONST)
    {
        return false;
    }

    //
    // If it's a variable, look it up in the symbol
    // table to see what type it is.
    //
    if (tac->tag == TAC_VAR)
    {
        Symbol *sym = stab_lookup(state->stab, tac->var.name);
        return sym->type->tag == TT_POINTER;
    }

    ICE_ASSERT(((void)"TAC node in codegen_operand_pointer was not an operand.", false));
    return false;
}

//
// Push an assembly instruction onto a code list.
//
static void codegen_push_instr(CodegenState *state, AsmNode *instr)
{
    list_push_back(&state->code, &instr->list);
}

//
// Push a static variable definition onto the statics list.
//
static void codegen_push_static(CodegenState *state, AsmNode *instr)
{
    list_push_back(state->statics, &instr->list);
}

static AsmOperand *codegen_expression(CodegenState *state, TacNode *tac);
static int codegen_type_align(TypeTable *tab, Type *type);

//
// Compute the alignment of a type system array.
//
static int codegen_type_array_align(TypeTable *tab, Type *type)
{
    ICE_ASSERT(type->tag == TT_ARRAY);

    if (type_size(tab, type) >= 16)
    {
        return 16;
    }

    return codegen_type_align(tab, type_array_element(type));
}

//
// Compute the alignment of a type system struct.
//
static int codegen_type_struct_align(TypeTable *tab, Type *type)
{
    ICE_ASSERT(type->tag == TT_STRUCT);

    TypetabEnt *ent = typetab_lookup(tab, type->strct.tag);
    return ent->struct_.align;
}

//
// Compute the alignment of a type system type.
//
static int codegen_type_align(TypeTable *tab, Type *type)
{
    switch (type->tag)
    {
    case TT_CHAR:
    case TT_UCHAR:
    case TT_SCHAR:
        return 1;

    case TT_INT:
        return 4;
    case TT_LONG:
        return 8;
    case TT_UINT:
        return 4;
    case TT_ULONG:
        return 8;
    case TT_DOUBLE:
        return 8;
    case TT_FUNC:
        ICE_ASSERT(((void)"function type passed to codegen_type_align", false));
    case TT_VOID:
        ICE_ASSERT(((void)"void type passed to codegen_type_align", false));
    case TT_POINTER:
        return 8;
    case TT_ARRAY:
        return codegen_type_array_align(tab, type);
    case TT_STRUCT:
        return codegen_type_struct_align(tab, type);
    }

    ICE_ASSERT(((void)"invalid type tag in codegen_type_align false", false));
    return 0;
}

//
// Convert an aggregate type to an byte array.
//
static AsmType *codegen_aggregate_to_asmtype(TypeTable *tab, Type *type)
{
    ICE_ASSERT(type->tag == TT_ARRAY || type->tag == TT_STRUCT);

    size_t size = type_size(tab, type);
    int align = codegen_type_align(tab, type);

    return asmtype_bytearray(size, align);
}

//
// Convert a type system type to the proper assembly type.
//
static AsmType *codegen_type_to_asmtype(TypeTable *tab, Type *type)
{
    ICE_ASSERT(type != NULL);

    switch (type->tag)
    {
    case TT_CHAR:
    case TT_UCHAR:
    case TT_SCHAR:
        return asmtype_byte();

    case TT_INT:
        return asmtype_long();
    case TT_UINT:
        return asmtype_long();
    case TT_LONG:
        return asmtype_quad();
    case TT_ULONG:
        return asmtype_quad();
    case TT_DOUBLE:
        return asmtype_double();
    case TT_FUNC:
        ICE_ASSERT(((void)"function type found in codegen_type_to_asmtype.", false));
    case TT_VOID:
        ICE_ASSERT(((void)"void type found in codegen_type_to_asmtype.", false));
    case TT_POINTER:
        return asmtype_quad();
    case TT_ARRAY:
    case TT_STRUCT:
        return codegen_aggregate_to_asmtype(tab, type);
    }

    ICE_ASSERT(false);
    return asmtype_long();
}

//
// Convert a TAC operand node to the proper assembly type.
//
// The TAC node must either be a variable or a constant.
//
static AsmType *codegen_tac_to_asmtype(CodegenState *state, TacNode *tac)
{
    //
    // If it's a constant, the size is specified.
    //
    if (tac->tag == TAC_CONST)
    {
        switch (tac->constant.tag)
        {
        case CON_INTEGRAL:
            switch (tac->constant.intval.size)
            {
            case CIS_CHAR:
                return asmtype_byte();
            case CIS_INT:
                return asmtype_long();
            case CIS_LONG:
                return asmtype_quad();
            }
            break;

        case CON_FLOAT:
            return asmtype_double();
        }

        ICE_ASSERT(((void)"invalid constant in codegen_tac_to_asmtype", false));
        return asmtype_long();
    }

    //
    // If it's a variable, look it up in the symbol
    // table to see what size it is.
    //
    if (tac->tag == TAC_VAR)
    {
        Symbol *sym = stab_lookup(state->stab, tac->var.name);
        return codegen_type_to_asmtype(state->typetab, sym->type);
    }

    ICE_ASSERT(((void)"TAC node in codegen_tac_to_asmtype was not an operand.", false));
    return asmtype_long();
}

//
//
// Return the alignment for a static variable.
//
static int codegen_align(TypeTable *typetab, Type *type)
{
    int align = 1;
    AsmType *at = codegen_type_to_asmtype(typetab, type);

    switch (at->tag)
    {
    case AT_BYTE:
        align = 1;
        break;
    case AT_LONGWORD:
        align = 4;
        break;
    case AT_QUADWORD:
        align = 8;
        break;
    case AT_DOUBLE:
        align = 8;
        break;
    case AT_BYTEARRAY:
        align = at->array.align;
        break;
    }

    asmtype_free(at);

    return align;
}

//
// Flatten out list of scalar types in an aggregate type, recursively.
// Caller is responsible for making sure the list is initially empty.
//
// List is of type TypeList.
//
static void codegen_flatten_struct(TypeTable *typetab, Type *type, List *out)
{
    if (type->tag == TT_STRUCT) {
        TypetabEnt *ent = typetab_lookup(typetab, type->strct.tag);

        for (ListNode *curr = ent->struct_.members.head; curr; curr = curr->next) {
            TypetabStructMember *member = CONTAINER_OF(curr, TypetabStructMember, list);
            codegen_flatten_struct(typetab, member->type, out);
        }
    } else if (type->tag == TT_ARRAY) {
        for (size_t i = 0; i < type->array.size; i++) {
            TypeList *tl = safe_zalloc(sizeof(TypeList));
            tl->type = type_clone(type->array.element);
            list_push_back(out, &tl->list);
        }
    } else {
        TypeList *tl = safe_zalloc(sizeof(TypeList));
        tl->type = type_clone(type);
        list_push_back(out, &tl->list);
    }
}

//
// Free a list of TypeList nodes.
//
static void codegen_free_typelist(List *list)
{
    ListNode *next = NULL;
    for (ListNode *curr = list->head; curr; curr = next) {
        next = curr->next;
        TypeList *tl = CONTAINER_OF(curr, TypeList, list);
        type_free(tl->type);
        safe_free(tl);
    }
}

//
// Classify a structure.
//
// The returned struct is allocated.
//
static ClassifiedStruct *codegen_classify_struct(TypeTable *typetab, Type *type)
{
    ICE_ASSERT(type->tag == TT_STRUCT);
    TypetabEnt *ent = typetab_lookup(typetab, type->strct.tag);

    size_t eightbytes = (ent->struct_.size + 7) / 8;
    ClassifiedStruct *cs = safe_zalloc(sizeof(ClassifiedStruct) + (eightbytes - 1) * sizeof(StructClass));

    cs->eightbytes = eightbytes;
    cs->size = ent->struct_.size;

    if (eightbytes > 2) {
        for (size_t i = 0; i < eightbytes; i++) {
            cs->sc[i] = STC_MEMORY;
        }
    } else {
        List types;
        list_clear(&types);
        codegen_flatten_struct(typetab, type, &types);

        if (eightbytes == 2) {
            ICE_ASSERT(types.head);
            ICE_ASSERT(types.tail);
            ICE_ASSERT(types.head != types.tail);

            Type *first = CONTAINER_OF(types.head, TypeList, list)->type;
            Type *last =  CONTAINER_OF(types.tail, TypeList, list)->type;

            if (first->tag == TT_DOUBLE && last->tag == TT_DOUBLE) {
                cs->sc[0] = STC_SSE;
                cs->sc[1] = STC_SSE;
            } else if (first->tag == TT_DOUBLE) {
                cs->sc[0] = STC_SSE;
                cs->sc[1] = STC_INTEGER;
            } else if (last->tag == TT_DOUBLE) {
                cs->sc[0] = STC_INTEGER;
                cs->sc[1] = STC_SSE;
            } else {
                cs->sc[0] = STC_INTEGER;
                cs->sc[1] = STC_INTEGER;
            }
        } else {
            ICE_ASSERT(eightbytes == 1);
            ICE_ASSERT(types.head);

            Type *first = CONTAINER_OF(types.head, TypeList, list)->type;

            if (first->tag == TT_DOUBLE) {
                cs->sc[0] = STC_SSE;
            } else {
                cs->sc[0] = STC_INTEGER;
            }
        }

        codegen_free_typelist(&types);
    }

    return cs;
}

//
// Clone an operand, which must be a pseudoreg or memory, and add an offset to the the 
// cloned operand.
//
static AsmOperand *clone_with_offset(AsmOperand *src, int offset)
{
    ICE_ASSERT(src->tag == AOP_PSEUDOREG || src->tag == AOP_MEMORY || src->tag == AOP_PSEUDOMEM);

    if (src->tag == AOP_PSEUDOREG) {
        return aoper_pseudomem(src->pseudoreg, offset);
    }

    if (src->tag == AOP_PSEUDOMEM) {
        return aoper_pseudomem(src->pseudomem.name, src->pseudomem.offset + offset);
    }

    return aoper_memory(src->memory.reg, src->memory.offset + offset);
}

//
// Generate code to copy memory. src and dst may be either pseudoreg or memory operands.
//
static void codegen_copy_bytes(CodegenState *state, AsmOperand *src, AsmOperand *dst, size_t bytes, FileLine loc)
{
    int offset = 0;

    while (bytes - offset >= 8) {
        codegen_push_instr(state, asm_mov(clone_with_offset(src, offset), clone_with_offset(dst, offset), asmtype_quad(), loc));
        offset += 8;
    }

    while (bytes - offset >= 4) {
        codegen_push_instr(state, asm_mov(clone_with_offset(src, offset), clone_with_offset(dst, offset), asmtype_long(), loc));
        offset += 4;
    }


    while (bytes > offset) {
        codegen_push_instr(state, asm_mov(clone_with_offset(src, offset), clone_with_offset(dst, offset), asmtype_byte(), loc));
        offset++;
    }
}

//
// Generate code to load memory from a pointer variable.
//
static void codegen_load_bytes(CodegenState *state, char *ptr, char *dst, size_t bytes, FileLine loc)
{
    codegen_push_instr(state, asm_mov(aoper_pseudoreg(ptr), aoper_reg(REG_RAX), asmtype_quad(), loc));
    
    int offset = 0;

    while (bytes - offset >= 8) {
        codegen_push_instr(state, asm_mov(aoper_memory(REG_RAX, offset), aoper_pseudomem(dst, offset), asmtype_quad(), loc));
        offset += 8;
    }

    while (bytes - offset >= 4) {
        codegen_push_instr(state, asm_mov(aoper_memory(REG_RAX, offset), aoper_pseudomem(dst, offset), asmtype_long(), loc));
        offset += 4;
    }


    while (bytes > offset) {
        codegen_push_instr(state, asm_mov(aoper_memory(REG_RAX, offset), aoper_pseudomem(dst, offset), asmtype_byte(), loc));
        offset++;
    }
}

//
// Generate code to store memory to a pointer variable.
//
static void codegen_store_bytes(CodegenState *state, char *src, char *ptr, size_t bytes, FileLine loc)
{
    codegen_push_instr(state, asm_mov(aoper_pseudoreg(ptr), aoper_reg(REG_RAX), asmtype_quad(), loc));
    
    int offset = 0;

    while (bytes - offset >= 8) {
        codegen_push_instr(state, asm_mov(aoper_pseudomem(src, offset), aoper_memory(REG_RAX, offset),asmtype_quad(), loc));
        offset += 8;
    }

    while (bytes - offset >= 4) {
        codegen_push_instr(state, asm_mov(aoper_pseudomem(src, offset), aoper_memory(REG_RAX, offset),asmtype_long(), loc));
        offset += 4;
    }


    while (bytes > offset) {
        codegen_push_instr(state, asm_mov(aoper_pseudomem(src, offset), aoper_memory(REG_RAX, offset),asmtype_byte(), loc));
        offset++;
    }
}

//
// Insert bytes from the source operand into a register.
//
static void codegen_copy_bytes_to_reg(CodegenState *state, AsmOperand *src, Register dst, size_t size, FileLine loc)
{
    int offset = size - 1;

    while (offset >= 0) {
        AsmOperand *srcoff = clone_with_offset(src, offset);
        codegen_push_instr(state, asm_mov(srcoff, aoper_reg(dst), asmtype_byte(), loc));
        if (offset > 0) {
            codegen_push_instr(state, asm_binary(BOP_LSHIFT, aoper_imm(8), aoper_reg(dst), asmtype_quad(), loc));        
        }   
        offset--;     
    }
}

//
// Insert bytes from a register into the dest operand.
//
static void codegen_copy_bytes_from_reg(CodegenState *state, Register src, AsmOperand *dst, size_t size, FileLine loc)
{
    int offset = 0;

    while (offset < size) {
        AsmOperand *dstoff = clone_with_offset(dst, offset);
        codegen_push_instr(state, asm_mov(aoper_reg(src), dstoff, asmtype_byte(), loc));
        if (offset < size - 1) {            
            codegen_push_instr(state, asm_binary(BOP_RSHIFT, aoper_imm(8), aoper_reg(src), asmtype_quad(), loc));        
        }
        offset++;
    }
}

//
// Create a labeled floating point literal and return the allocated label.
//
static char *codegen_float_literal(CodegenState *state, double floatval, int align, FileLine loc)
{
    char *val = saprintf("float_%g", floatval);
    for (char *p = val; *p; p++)
    {
        //
        // Make value safe as a label
        //
        if (!(isalpha(*p) || isdigit(*p) || *p == '_' || *p == '.'))
        {
            *p = '_';
        }
    }

    char *label = tmp_name(val);
    safe_free(val);

    StaticInitializer *si = sinit_make_const(const_make_double(floatval));
    codegen_push_static(state, asm_static_const(label, align, si, loc));

    BackEndSymbol *sym = bstab_lookup(state->bstab, label);
    sym->tag = BST_OBJECT;
    sym->object.type = asmtype_double();
    sym->object.is_static = true;
    sym->object.is_literal = true;

    return label;
}

//
// Generate code for a constant.
//
static AsmOperand *codegen_constant(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_CONST);

    if (tac->constant.tag == CON_FLOAT)
    {
        char *label = codegen_float_literal(state, tac->constant.floatval, 8, tac->loc);
        return aoper_data(label, 0);
    }

    return aoper_imm(tac->constant.intval.value);
}

//
// Generate code for a variable reference.
//
static AsmOperand *codegen_var(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_VAR);

    Symbol *sym = stab_lookup(state->stab, tac->var.name);

    if (sym->type->tag == TT_ARRAY)
    {
        return aoper_pseudomem(tac->var.name, 0);
    }

    return aoper_pseudoreg(tac->var.name);
}

//
// Generate code for an expression. Code to generate the value
// will be appended to state->code, and an unreferenced operand
// node containing the result location will be returned.
//
static AsmOperand *codegen_expression(CodegenState *state, TacNode *tac)
{
    switch (tac->tag)
    {
    case TAC_CONST:
        return codegen_constant(state, tac);
    case TAC_VAR:
        return codegen_var(state, tac);

    default:
        ICE_ASSERT(("invalid TAC node in codegen_expression"));
    }

    //
    // never reached
    //
    return NULL;
}

//
// Generate code for a unary not operator.
//
static void codegen_unary_not(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_UNARY);
    ICE_ASSERT(tac->unary.op == UOP_LOGNOT);

    AsmOperand *src = codegen_expression(state, tac->unary.src);
    AsmOperand *dst = codegen_expression(state, tac->unary.dst);

    AsmType *srctype = codegen_tac_to_asmtype(state, tac->unary.src);
    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->unary.dst);

    if (srctype->tag == AT_DOUBLE)
    {
        codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_reg(REG_XMM13), aoper_reg(REG_XMM13), asmtype_double(), tac->loc));
        codegen_push_instr(state, asm_cmp(aoper_reg(REG_XMM13), src, srctype, tac->loc));
    }
    else
    {
        codegen_push_instr(state, asm_cmp(aoper_imm(0), src, srctype, tac->loc));
    }

    codegen_push_instr(state, asm_mov(aoper_imm(0), dst, dsttype, tac->loc));
    codegen_push_instr(state, asm_setcc(aoper_clone(dst), ACC_E, tac->loc));
}

//
// Generate code for a unary negation on floating point.
//
static void codegen_unary_neg_float(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_UNARY);
    ICE_ASSERT(tac->unary.op == UOP_MINUS);

    AsmOperand *src = codegen_expression(state, tac->unary.src);
    AsmOperand *dst = codegen_expression(state, tac->unary.dst);

    if (state->neg_zero == NULL)
    {
        state->neg_zero = codegen_float_literal(state, -0.0, 16, tac->loc);
    }

    codegen_push_instr(state, asm_mov(src, dst, asmtype_double(), tac->loc));
    codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_data(state->neg_zero, 0), aoper_clone(dst), asmtype_double(), tac->loc));
}

//
// Generate code for a unary operator.
//
static void codegen_unary(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_UNARY);

    if (tac->unary.op == UOP_LOGNOT)
    {
        codegen_unary_not(state, tac);
        return;
    }

    if (tac->unary.op == UOP_MINUS && codegen_operand_float(state, tac->unary.dst))
    {
        codegen_unary_neg_float(state, tac);
        return;
    }

    AsmOperand *src = codegen_expression(state, tac->unary.src);
    AsmOperand *dst = codegen_expression(state, tac->unary.dst);

    AsmType *srctype = codegen_tac_to_asmtype(state, tac->unary.src);
    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->unary.dst);
    codegen_push_instr(state, asm_mov(src, dst, srctype, tac->loc));
    codegen_push_instr(state, asm_unary(tac->unary.op, aoper_clone(dst), dsttype, tac->loc));
}

//
// Generate code for a relational operator.
//
static void codegen_relational(CodegenState *state, TacNode *tac)
{
    AsmConditionCode cc = ACC_E;

    bool is_unsigned = codegen_operand_unsigned(state, tac->binary.left);
    bool is_float = codegen_operand_float(state, tac->binary.left);
    bool is_pointer =
        codegen_operand_pointer(state, tac->binary.left) ||
        codegen_operand_pointer(state, tac->binary.right);

    if (is_unsigned || is_float || is_pointer)
    {
        switch (tac->binary.op)
        {
        case BOP_EQUALITY:
            cc = ACC_E;
            break;
        case BOP_NOTEQUAL:
            cc = ACC_NE;
            break;
        case BOP_LESSTHAN:
            cc = ACC_B;
            break;
        case BOP_GREATERTHAN:
            cc = ACC_A;
            break;
        case BOP_LESSEQUAL:
            cc = ACC_BE;
            break;
        case BOP_GREATEREQUAL:
            cc = ACC_AE;
            break;

        default:
            ICE_ASSERT(((void)"invalid binary op in codegen_relational", false));
        }
    }
    else
    {
        switch (tac->binary.op)
        {
        case BOP_EQUALITY:
            cc = ACC_E;
            break;
        case BOP_NOTEQUAL:
            cc = ACC_NE;
            break;
        case BOP_LESSTHAN:
            cc = ACC_L;
            break;
        case BOP_GREATERTHAN:
            cc = ACC_G;
            break;
        case BOP_LESSEQUAL:
            cc = ACC_LE;
            break;
        case BOP_GREATEREQUAL:
            cc = ACC_GE;
            break;

        default:
            ICE_ASSERT(((void)"invalid binary op in codegen_relational", false));
        }
    }

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    AsmType *cmptype = codegen_tac_to_asmtype(state, tac->binary.left);
    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->binary.dst);

    codegen_push_instr(state, asm_cmp(right, left, cmptype, tac->loc));
    codegen_push_instr(state, asm_mov(aoper_imm(0), dst, dsttype, tac->loc));
    codegen_push_instr(state, asm_setcc(aoper_clone(dst), cc, tac->loc));
}

//
// Generate code for an integer divide (or modulo) operation.
//
static void codegen_int_div(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_BINARY);
    ICE_ASSERT(tac->binary.op == BOP_DIVIDE || tac->binary.op == BOP_MODULO);

    bool is_unsigned = codegen_operand_unsigned(state, tac->binary.left);
    AsmType *lefttype = codegen_tac_to_asmtype(state, tac->binary.left);

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    codegen_push_instr(state, asm_mov(left, aoper_reg(REG_RAX), lefttype, tac->loc));
    if (is_unsigned)
    {
        lefttype = asmtype_clone(lefttype);
        codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_reg(REG_RDX), aoper_reg(REG_RDX), lefttype, tac->loc));
        lefttype = asmtype_clone(lefttype);
        codegen_push_instr(state, asm_div(right, lefttype, tac->loc));
    }
    else
    {
        lefttype = asmtype_clone(lefttype);
        codegen_push_instr(state, asm_cdq(lefttype, tac->loc));
        lefttype = asmtype_clone(lefttype);
        codegen_push_instr(state, asm_idiv(right, lefttype, tac->loc));
    }

    //
    // div/idiv return quotient in AX, remainder in DX
    //
    Register dstreg = tac->binary.op == BOP_DIVIDE ? REG_RAX : REG_RDX;
    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->binary.left);
    codegen_push_instr(state, asm_mov(aoper_reg(dstreg), dst, dsttype, tac->loc));
}

//
// Generate code for a floating point divide.
//
static void codegen_float_div(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_BINARY);
    ICE_ASSERT(tac->binary.op == BOP_DIVIDE); // NOTE: no modulo in floating point.

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    codegen_push_instr(state, asm_mov(left, dst, asmtype_double(), tac->loc));
    codegen_push_instr(state, asm_binary(BOP_DIVDBL, right, aoper_clone(dst), asmtype_double(), tac->loc));
}

//
// Generate code for a binary operator.
//
static void codegen_binary(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_BINARY);

    if (bop_is_relational(tac->binary.op))
    {
        codegen_relational(state, tac);
        return;
    }

    if (tac->binary.op == BOP_DIVIDE || tac->binary.op == BOP_MODULO)
    {
        if (codegen_operand_float(state, tac->binary.dst))
        {
            codegen_float_div(state, tac);
        }
        else
        {
            codegen_int_div(state, tac);
        }
        return;
    }

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->binary.left);
    codegen_push_instr(state, asm_mov(left, dst, dsttype, tac->loc));
    dst = aoper_clone(dst);
    dsttype = asmtype_clone(dsttype);
    codegen_push_instr(state, asm_binary(tac->binary.op, right, dst, dsttype, tac->loc));
}

//
// Generate code for a return instruction.
//
static void codegen_return(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_RETURN);

    static Register int_regs[2] = { REG_RAX, REG_RDX };
    static Register flt_regs[2] = { REG_XMM0, REG_XMM1 };

    if (tac->ret.val) {
        AsmOperand *retval = codegen_expression(state, tac->ret.val);
        AsmType *rettype = codegen_tac_to_asmtype(state, tac->ret.val);

        List ints;
        List flts;
        bool return_in_memory = classify_return_value(state, tac->ret.val, &ints, &flts);

        if (return_in_memory) {
            codegen_push_instr(state, asm_mov(aoper_memory(REG_RBP, -8), aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
            codegen_copy_bytes(state, aoper_memory(REG_RAX, 0), retval, rettype->array.size, tac->loc); 
        } else {
            int reg_index = 0;

            ListNode *next = NULL;
            for (ListNode *curr = ints.head; curr; curr = next, reg_index++) {
                next = curr->next;
                TypedOperand *to = CONTAINER_OF(curr, TypedOperand, list);
                if (to->type->tag == AT_BYTEARRAY) {
                    codegen_copy_bytes_to_reg(state, to->oper, int_regs[reg_index], to->type->array.size, tac->loc);
                } else {
                    codegen_push_instr(state, asm_mov(to->oper, aoper_reg(int_regs[reg_index]), to->type, tac->loc));
                }
                safe_free(to);
            }

            next = NULL;
            reg_index = 0;
            for (ListNode *curr = flts.head; curr; curr = next, reg_index++) {
                next = curr->next;
                TypedOperand *to = CONTAINER_OF(curr, TypedOperand, list);

                codegen_push_instr(state, asm_mov(to->oper, aoper_reg(flt_regs[reg_index]), to->type, tac->loc));

                safe_free(to);
            }
        }
    }
    codegen_push_instr(state, asm_ret(tac->loc));
}

//
// Generate code for a jump instruction.
//
static void codegen_jump(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_JUMP);

    codegen_push_instr(state, asm_jump(tac->jump.target, tac->loc));
}

//
// Generate code for a jump-on-zero instruction.
//
static void codegen_jump_zero(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_JUMP_ZERO);

    AsmOperand *cond = codegen_expression(state, tac->jump_zero.condition);
    AsmType *condtype = codegen_tac_to_asmtype(state, tac->jump_zero.condition);

    if (condtype->tag == AT_DOUBLE)
    {
        codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_reg(REG_XMM13), aoper_reg(REG_XMM13), condtype, tac->loc));
        codegen_push_instr(state, asm_cmp(cond, aoper_reg(REG_XMM13), asmtype_clone(condtype), tac->loc));
    }
    else
    {
        codegen_push_instr(state, asm_cmp(aoper_imm(0), cond, condtype, tac->loc));
    }
    codegen_push_instr(state, asm_jumpcc(tac->jump_zero.target, ACC_E, tac->loc));
}

//
// Generate code for a jump-on-not-zero instruction.
//
static void codegen_jump_not_zero(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_JUMP_NOT_ZERO);

    AsmOperand *cond = codegen_expression(state, tac->jump_not_zero.condition);
    AsmType *condtype = codegen_tac_to_asmtype(state, tac->jump_not_zero.condition);

    if (condtype->tag == AT_DOUBLE)
    {
        codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_reg(REG_XMM13), aoper_reg(REG_XMM13), condtype, tac->loc));
        codegen_push_instr(state, asm_cmp(cond, aoper_reg(REG_XMM13), asmtype_clone(condtype), tac->loc));
    }
    else
    {
        codegen_push_instr(state, asm_cmp(aoper_imm(0), cond, condtype, tac->loc));
    }
    codegen_push_instr(state, asm_jumpcc(tac->jump_zero.target, ACC_NE, tac->loc));
}

//
// Generate code for a copy instruction.
//
static void codegen_copy(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_COPY);

    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->copy.dst);
    AsmOperand *src = codegen_expression(state, tac->copy.src);
    AsmOperand *dst = codegen_expression(state, tac->copy.dst);

    if (dsttype->tag == AT_BYTEARRAY) {        
        codegen_copy_bytes(state, src, dst, dsttype->array.size, tac->loc);
        return;
    }

    
    codegen_push_instr(state, asm_mov(src, dst, dsttype, tac->loc));
}

//
// Generate code for a label.
//
static void codegen_label(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_LABEL);

    codegen_push_instr(state, asm_label(tac->label.name, tac->loc));
}

//
// Construct a typed operand and push it onto a list.
//
static void push_typed_operand(List *list, AsmType *type, AsmOperand *oper)
{
    TypedOperand *to = safe_zalloc(sizeof(TypedOperand));
    to->type = type;
    to->oper = oper;
    list_push_back(list, &to->list);
}

//
// For structure pieces, get the type of the current 8byte based on how
// much structure is left.
//
static AsmType *get_eightbyte_type(size_t offset, size_t structsize)
{
    size_t bytes_left = structsize - offset;

    if (bytes_left >= 8) {
        return asmtype_quad();
    } else if (bytes_left == 4) {
        return asmtype_long();
    } else if (bytes_left == 1) {
        return asmtype_byte();
    }
    return asmtype_bytearray(bytes_left, 8);
}

//
// Classify a struct parameter into int/float registers or on the stack, and
// populate the pieces of the struct into the proper parameter list.
//
static void codegen_classify_struct_parameter(CodegenState *state, char *var, ClassifyParmsState *cpstate, Type *structtype)
{
    ClassifiedStruct *cs = codegen_classify_struct(state->typetab, structtype);

    bool use_stack = true;
    size_t struct_size = cs->size;

    if (cs->sc[0] != STC_MEMORY) {
        int tent_ints = 0;
        int tent_floats = 0;

        //
        // First count classes and see if everything fits in remaining
        // registers.
        //
        for (size_t i = 0; i < cs->eightbytes; i++) {
            switch (cs->sc[i]) {
                case STC_INTEGER:
                    tent_ints++;
                    break;

                case STC_SSE:
                    tent_floats++;
                    break;

                case STC_MEMORY:
                    ICE_ASSERT(((void)"memory eightbyte mixed with scalars", false));
                    break;
            }
        }

        if (tent_ints <= cpstate->intregs && tent_floats <= float_arg_reg_count) {
            // 
            // Fits in registers, add to lists.
            //
            size_t offset = 0;
            for (size_t i = 0; i < cs->eightbytes; i++) {
                AsmOperand *oper = aoper_pseudomem(var, offset);
                AsmType *type = get_eightbyte_type(offset, struct_size);

                switch (cs->sc[i]) {
                    case STC_INTEGER:
                        push_typed_operand(cpstate->ints, type, oper);
                        cpstate->nints++;
                        break;

                    case STC_SSE:
                        push_typed_operand(cpstate->floats, type, oper);
                        cpstate->nfloats++;
                        break;

                    case STC_MEMORY:
                        ICE_ASSERT(((void)"memory eightbyte mixed with scalars", false));
                        break;
                }

                offset += 8;
            }

            use_stack = false;
        }
    }

    if (use_stack) {
        // 
        // Does not fit in registers, add to stack list.
        //
        size_t offset = 0;
        for (size_t i = 0; i < cs->eightbytes; i++) {
            AsmOperand *oper = aoper_pseudomem(var, offset);
            AsmType *type = get_eightbyte_type(offset, struct_size);
            push_typed_operand(cpstate->stack, type, oper);
            offset += 8;
        }
    }

    safe_free(cs);
}

//
// Given a list of TAC operand nodes, partition the list into new lists based on
// parameter passing conventions. The returned lists contain <TypeOperand> node.
//
// - First 6 integer operands go into integer registers.
// - First 8 float operands go into XMM registers.
// - Remaining operands go onto the stack.
//
// The original list will be destroyed.
//
static void codegen_classify_parameters(CodegenState *state, List *nodes, List *ints, List *floats, List *stack, bool return_in_memory)
{
    ClassifyParmsState cpstate;

    cpstate.nints = 0;
    cpstate.nfloats = 0;
    cpstate.ints = ints;
    cpstate.floats = floats;
    cpstate.stack = stack;

    list_clear(cpstate.ints);
    list_clear(cpstate.floats);
    list_clear(cpstate.stack);

    cpstate.intregs = int_arg_reg_count;
    if (return_in_memory) {
        cpstate.intregs--;
    }

    ListNode *next = NULL;
    for (ListNode *curr = nodes->head; curr; curr = next) {
        next = curr->next;

        TacNode *parm = CONTAINER_OF(curr, TacNode, list);
        
        enum ParmType {
            Double,
            Scalar,
            Struct
        } parmtype = Scalar;

        Type *structtype = NULL;

        if (parm->tag == TAC_CONST) {
            if (parm->constant.tag != CON_FLOAT) {
                parmtype = Scalar;
            } else {
                parmtype = Double;
            }
        } else if (parm->tag == TAC_VAR) {
            Symbol *sym = stab_lookup(state->stab, parm->var.name);
            if (sym->type->tag == TT_DOUBLE) {
                parmtype = Double;
            } else if (type_scalar(sym->type)) {
                parmtype = Scalar;
            } else {
                ICE_ASSERT(sym->type->tag == TT_STRUCT);
                parmtype = Struct;
                structtype = sym->type;
            }
        } else {
            ICE_ASSERT(((void)"function operand not const or var", false));
            return;
        }

        AsmOperand *oper = codegen_expression(state, parm);
        AsmType *at = codegen_tac_to_asmtype(state, parm);

        switch (parmtype) {
            case Double:
                if (cpstate.nfloats < float_arg_reg_count) {
                    push_typed_operand(cpstate.floats, at, oper);
                    cpstate.nfloats++;
                } else {
                    push_typed_operand(cpstate.stack, at, oper);
                }
                break;

            case Scalar:
                if (cpstate.nints < cpstate.intregs) {
                    push_typed_operand(cpstate.ints, at, oper);
                    cpstate.nints++;
                } else {
                    push_typed_operand(cpstate.stack, at, oper);
                }
                break;

            case Struct:
                ICE_ASSERT(parm->tag = TAC_VAR);
                codegen_classify_struct_parameter(state, parm->var.name, &cpstate, structtype);
                break;
        }        
    }
}

//
// Classify how a return value is to be returned: in registers or in memory.
// If in registers, populate the `ints` and `doubles` lists with the 
// registers to use.
//
// Returns true if the value is returned in memory; else false.
//
static bool classify_return_value(CodegenState *state, TacNode *retval, List *ints, List *doubles)
{
    AsmType *at = codegen_tac_to_asmtype(state, retval);

    list_clear(ints);
    list_clear(doubles);

    if (at->tag == AT_DOUBLE) {
        AsmOperand *oper = codegen_expression(state, retval);
        push_typed_operand(doubles, at, oper);
        return false;
    }

    if (at->tag != AT_BYTEARRAY) {
        //
        // scalar
        //
        AsmOperand *oper = codegen_expression(state, retval);
        push_typed_operand(ints, at, oper);
        return false;
    }

    //
    // Must be a struct
    //
    ICE_ASSERT(retval->tag == TAC_VAR);
    char *var = retval->var.name;
    Symbol *sym = stab_lookup(state->stab, var);
    ICE_ASSERT(sym->type->tag == TT_STRUCT);
    ClassifiedStruct *cs = codegen_classify_struct(state->typetab, sym->type);

    asmtype_free(at);

    bool return_in_memory = true;

    if (cs->sc[0] != STC_MEMORY) {
        size_t offset = 0;
        for (size_t i = 0; i < cs->eightbytes; i++) {            
            AsmOperand *oper = aoper_pseudomem(var, offset);
            AsmType *at = get_eightbyte_type(offset, cs->size);

            switch (cs->sc[i]) {
                case STC_INTEGER:
                    push_typed_operand(ints, at, oper);
                    break;

                case STC_SSE:
                    push_typed_operand(doubles, at, oper);
                    break;

                case STC_MEMORY:
                    ICE_ASSERT(((void)"STC_MEMORY found in classify_return_value", false));
            }
            offset += 8;
        }
        return_in_memory = false;
    }

    safe_free(cs);
    return return_in_memory;
}

//
// Check if a return value would be in memory without building the
// parameter lists.
//
static bool check_return_in_memory(TypeTable *typetab, Type *rettype)
{
    if (rettype->tag != TT_STRUCT) {
        return false;
    }

    ClassifiedStruct *cs = codegen_classify_struct(typetab, rettype);
    bool return_in_memory = cs->sc[0] == STC_MEMORY;
    safe_free(cs);

    return return_in_memory;
}

//
// Generate code for a function call.
//
// The stack pointer on entry is guaranteed to be 16-byte aligned; it must
// maintain this alignment at the call point.
//
// The calling convention takes the first 6 integer arguments in registers;
// following arguments are pushed in reverse order on the stack. The caller
// is responsible for stack cleanup.
//
static void codegen_function_call(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_FUNCTION_CALL);
    TacFunctionCall *call = &tac->call;

    List ints, floats, stack;

    List int_dests;
    List double_dests;
    bool return_in_memory = false;
    
    if (call->dst) {
        return_in_memory = classify_return_value(state, call->dst, &int_dests, &double_dests);
    }

    int intreg = 0;

    if (return_in_memory) {
        AsmOperand *dstop = codegen_expression(state, call->dst);
        codegen_push_instr(state, asm_lea(dstop, aoper_reg(REG_RDI), tac->loc));
        intreg++;
    }

    codegen_classify_parameters(state, &call->args, &ints, &floats, &stack, return_in_memory);

    int args_on_stack = list_count(&stack);

    //
    // We push 8-byte arguments on the stack, but the stack pointer must stay
    // 16-byte aligned; if there are an odd number of stack arguments, insert
    // 8 bytes of padding.
    //
    int stack_padding = (args_on_stack & 1) ? 8 : 0;

    if (stack_padding) {
        codegen_push_instr(state, asm_stack_reserve(stack_padding, tac->loc));
    }

    //
    // Load integer register arguments.
    //
    ListNode *curr = ints.head;
    ListNode *next = NULL;
    for (int i = return_in_memory ? 1 : 0; curr; i++, curr = next) {
        next = curr->next;
        TypedOperand *to = CONTAINER_OF(curr, TypedOperand, list);
        Register arg_reg = int_arg_regs[i];
        
        if (to->type->tag == AT_BYTEARRAY) {
            codegen_copy_bytes_to_reg(state, to->oper, arg_reg, to->type->array.size, tac->loc);
        } else {
            codegen_push_instr(state, asm_mov(to->oper, aoper_reg(arg_reg), to->type, tac->loc));
        }        
        safe_free(to);
    }

    //
    // Load floating point register arguments.
    //
    curr = floats.head;
    next = NULL;
    for (int i = 0; curr; i++, curr = next) {
        next = curr->next;
        TypedOperand *to = CONTAINER_OF(curr, TypedOperand, list);
        Register arg_reg = float_arg_regs[i];
        codegen_push_instr(state, asm_mov(to->oper, aoper_reg(arg_reg), to->type, tac->loc));
        safe_free(to);
    }

    //
    // Push stack args.
    //
    list_reverse(&stack);

    curr = stack.head;
    next = NULL;
    for (int i = 0; i < args_on_stack; i++, curr = next) {
        next = curr->next;
        TypedOperand *to = CONTAINER_OF(curr, TypedOperand, list);
        if (to->type->tag == AT_BYTEARRAY) {
            codegen_push_instr(state, asm_binary(BOP_SUBTRACT, aoper_imm(8), aoper_reg(REG_RSP), asmtype_quad(), tac->loc));
            codegen_copy_bytes(state, to->oper, aoper_memory(REG_RSP, 0), to->type->array.size, tac->loc);
        } else if (to->type->tag == AT_DOUBLE) {
            codegen_push_instr(state, asm_push(to->oper, tac->loc));
        } else {
            codegen_push_instr(state, asm_mov(to->oper, aoper_reg(REG_RAX), to->type, tac->loc));
            codegen_push_instr(state, asm_push(aoper_reg(REG_RAX), tac->loc));
        }
        safe_free(to);
    }

    //
    // Do the call
    //
    codegen_push_instr(state, asm_call(call->name, tac->loc));

    //
    // Clean up
    //
    int bytes_to_free = stack_padding + 8 * args_on_stack;
    if (bytes_to_free) {
        codegen_push_instr(state, asm_stack_free((bytes_to_free), tac->loc));
    }

    //
    // Return value, if any and not in memory.
    //
    if (call->dst && !return_in_memory) {
        Register int_ret_reg[2] = { REG_RAX, REG_RDX };
        Register flt_ret_reg[2] = { REG_XMM0, REG_XMM1 };

        int reg_index = 0;

        ListNode *next = NULL;
        for (ListNode *curr = int_dests.head; curr; curr = next, reg_index++) {
            next = curr->next;
            TypedOperand *to = CONTAINER_OF(curr, TypedOperand, list);
            if (to->type->tag == AT_BYTEARRAY) {
                codegen_copy_bytes_from_reg(state, int_ret_reg[reg_index], to->oper, to->type->array.size, tac->loc);
            } else {
                codegen_push_instr(state, asm_mov(aoper_reg(int_ret_reg[reg_index]), to->oper, to->type, tac->loc));
            }
            safe_free(to);
        }

        reg_index = 0;
        next = NULL;
        for (ListNode *curr = double_dests.head; curr; curr = next, reg_index++) {
            next = curr->next;
            TypedOperand *to = CONTAINER_OF(curr, TypedOperand, list);
            if (to->type->tag == AT_BYTEARRAY) {
                codegen_copy_bytes_from_reg(state, flt_ret_reg[reg_index], to->oper, to->type->array.size, tac->loc);
            } else {
                codegen_push_instr(state, asm_mov(aoper_reg(flt_ret_reg[reg_index]), to->oper, to->type, tac->loc));
            }
            safe_free(to);
        }
    }
}

//
// Geneate code for a static variable.
//
static void codegen_static_var(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_STATIC_VAR);

    codegen_push_static(state,
                        asm_static_var(
                            decl->static_var.name,
                            decl->static_var.global,
                            codegen_align(state->typetab, decl->static_var.type),
                            decl->static_var.init,
                            decl->loc));
}

//
// Geneate code for a static constant.
//
static void codegen_static_const(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_STATIC_CONST);

    codegen_push_static(state,
                        asm_static_const(
                            decl->static_const.name,
                            codegen_align(state->typetab, decl->static_const.type),
                            decl->static_const.init,
                            decl->loc));
}

//
// Generate code for a sign extend operation.
//
static void codegen_sign_extend(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_SIGN_EXTEND);

    codegen_push_instr(state,
                       asm_movsx(
                           codegen_tac_to_asmtype(state, decl->sign_extend.src),
                           codegen_tac_to_asmtype(state, decl->sign_extend.dst),
                           codegen_expression(state, decl->sign_extend.src),
                           codegen_expression(state, decl->sign_extend.dst),
                           decl->loc));
}

//
// Generate code for a zero extend operation.
//
static void codegen_zero_extend(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_ZERO_EXTEND);

    codegen_push_instr(state,
                       asm_movzx(
                           codegen_tac_to_asmtype(state, decl->zero_extend.src),
                           codegen_tac_to_asmtype(state, decl->zero_extend.dst),
                           codegen_expression(state, decl->zero_extend.src),
                           codegen_expression(state, decl->zero_extend.dst),
                           decl->loc));
}

//
// Generate code for a truncate operation.
//
static void codegen_truncate(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_TRUNCATE);

    codegen_push_instr(state,
                       asm_mov(
                           codegen_expression(state, decl->truncate.src),
                           codegen_expression(state, decl->truncate.dst),
                           codegen_tac_to_asmtype(state, decl->truncate.dst),
                           decl->loc));
}

//
// Generate code to convert a double to an integer.
//
static void codegen_dbl_to_int(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_DOUBLE_TO_INT);

    AsmOperand *src = codegen_expression(state, tac->dbl_to_int.src);
    AsmOperand *dst = codegen_expression(state, tac->dbl_to_int.dst);
    AsmType *type = codegen_tac_to_asmtype(state, tac->dbl_to_int.dst);

    if (type->tag == AT_BYTE)
    {
        codegen_push_instr(state,
                           asm_cvttsd2si(src, aoper_reg(REG_RAX), asmtype_long(), tac->loc));
        codegen_push_instr(state,
                           asm_mov(aoper_reg(REG_RAX), dst, type, tac->loc));
    }
    else
    {
        codegen_push_instr(state,
                           asm_cvttsd2si(src, dst, type, tac->loc));
    }
}

//
// Generate code to convert a double to an unsigned integer.
//
static void codegen_dbl_to_uint(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_DOUBLE_TO_UINT);

    AsmOperand *src = codegen_expression(state, tac->dbl_to_uint.src);
    AsmOperand *dst = codegen_expression(state, tac->dbl_to_uint.dst);
    AsmType *type = codegen_tac_to_asmtype(state, tac->dbl_to_uint.dst);

    if (type->tag == AT_BYTE)
    {
        codegen_push_instr(state,
                           asm_cvttsd2si(aoper_clone(src), aoper_reg(REG_RAX), asmtype_long(), tac->loc));
        codegen_push_instr(state,
                           asm_mov(aoper_reg(REG_RAX), dst, type, tac->loc));
    }
    else if (type->tag == AT_LONGWORD)
    {
        codegen_push_instr(state, asm_cvttsd2si(aoper_clone(src), aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_mov(aoper_reg(REG_RAX), aoper_clone(dst), asmtype_long(), tac->loc));
    }
    else
    {
        if (state->dbl_to_uint_ub == NULL)
        {
            state->dbl_to_uint_ub = codegen_float_literal(state, DBL_TO_UINT_UPPER_FLT, 16, tac->loc);
        }

        char *label1 = tmp_name("label");
        char *label2 = tmp_name("label");

        //
        // This algorithm avoids rounding error in the conversion by adjusting the
        // value to convert if it's over the max that can be represented in a signed long.
        //
        codegen_push_instr(state, asm_cmp(aoper_clone(src), aoper_data(state->dbl_to_uint_ub, 0), asmtype_double(), tac->loc));
        codegen_push_instr(state, asm_jumpcc(label1, ACC_AE, tac->loc));
        codegen_push_instr(state, asm_cvttsd2si(aoper_clone(src), aoper_clone(dst), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_jump(label2, tac->loc));
        codegen_push_instr(state, asm_label(label1, tac->loc));
        codegen_push_instr(state, asm_mov(aoper_clone(src), aoper_reg(REG_XMM13), asmtype_double(), tac->loc));
        codegen_push_instr(state, asm_binary(BOP_SUBTRACT, aoper_data(state->dbl_to_uint_ub, 0), aoper_reg(REG_XMM13), asmtype_double(), tac->loc));
        codegen_push_instr(state, asm_cvttsd2si(aoper_reg(REG_XMM13), aoper_clone(dst), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_mov(aoper_imm(DBL_TO_UINT_UPPER_INT), aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_binary(BOP_ADD, aoper_reg(REG_RAX), aoper_clone(dst), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_label(label2, tac->loc));

        safe_free(label1);
        safe_free(label2);
    }

    asmtype_free(type);
    aoper_free(src);
    aoper_free(dst);
}

//
// Generate code to convert an integer to a double.
//
static void codegen_int_to_dbl(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_INT_TO_DOUBLE);

    AsmOperand *src = codegen_expression(state, tac->int_to_dbl.src);
    AsmOperand *dst = codegen_expression(state, tac->int_to_dbl.dst);
    AsmType *type = codegen_tac_to_asmtype(state, tac->int_to_dbl.src);

    if (type->tag == AT_BYTE)
    {
        codegen_push_instr(state, asm_movsx(type, asmtype_long(), src, aoper_reg(REG_RAX), tac->loc));
        codegen_push_instr(state, asm_cvtsi2sd(aoper_reg(REG_RAX), dst, asmtype_long(), tac->loc));
    }
    else
    {
        codegen_push_instr(state, asm_cvtsi2sd(src, dst, type, tac->loc));
    }
}

//
// Generate code to convert an unsigned integer to a double.
//
static void codegen_uint_to_dbl(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_UINT_TO_DOUBLE);

    AsmOperand *src = codegen_expression(state, tac->uint_to_dbl.src);
    AsmOperand *dst = codegen_expression(state, tac->uint_to_dbl.dst);
    AsmType *type = codegen_tac_to_asmtype(state, tac->uint_to_dbl.src);

    if (type->tag == AT_BYTE)
    {
        codegen_push_instr(state, asm_movzx(asmtype_clone(type), asmtype_long(), aoper_clone(src), aoper_reg(REG_RAX), tac->loc));
        codegen_push_instr(state, asm_cvtsi2sd(aoper_reg(REG_RAX), aoper_clone(dst), asmtype_quad(), tac->loc));
    }
    else if (type->tag == AT_LONGWORD)
    {
        codegen_push_instr(state, asm_movzx(asmtype_clone(type), asmtype_quad(), aoper_clone(src), aoper_reg(REG_RAX), tac->loc));
        codegen_push_instr(state, asm_cvtsi2sd(aoper_reg(REG_RAX), aoper_clone(dst), asmtype_quad(), tac->loc));
    }
    else
    {
        char *label1 = tmp_name("label");
        char *label2 = tmp_name("label");

        codegen_push_instr(state, asm_cmp(aoper_imm(0), aoper_clone(src), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_jumpcc(label1, ACC_L, tac->loc));
        codegen_push_instr(state, asm_cvtsi2sd(aoper_clone(src), aoper_clone(dst), asmtype_clone(type), tac->loc));
        codegen_push_instr(state, asm_jump(label2, tac->loc));
        codegen_push_instr(state, asm_label(label1, tac->loc));
        codegen_push_instr(state, asm_mov(aoper_clone(src), aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_mov(aoper_reg(REG_RAX), aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_unary(UOP_SHR, aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_binary(BOP_BITAND, aoper_imm(1), aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_binary(BOP_BITOR, aoper_reg(REG_RAX), aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_cvtsi2sd(aoper_reg(REG_RDX), aoper_clone(dst), asmtype_clone(type), tac->loc));
        codegen_push_instr(state, asm_binary(BOP_ADD, aoper_clone(dst), aoper_clone(dst), asmtype_double(), tac->loc));
        codegen_push_instr(state, asm_label(label2, tac->loc));

        safe_free(label1);
        safe_free(label2);
    }

    asmtype_free(type);
    aoper_free(src);
    aoper_free(dst);
}

//
// Generate code for a get-address operation.
//
static void codegen_get_address(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_GET_ADDRESS);

    AsmOperand *src = codegen_expression(state, tac->get_address.src);
    AsmOperand *dst = codegen_expression(state, tac->get_address.dst);
    codegen_push_instr(state, asm_lea(src, dst, tac->loc));
}

//
// Generate code for a load operation.
//
static void codegen_load(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_LOAD);

    AsmType *type = codegen_tac_to_asmtype(state, tac->load.dst);

    if (type->tag == AT_BYTEARRAY && tac->load.src->tag == TAC_VAR && tac->load.dst->tag == TAC_VAR) {
        codegen_load_bytes(state, tac->load.src->var.name, tac->load.dst->var.name, type->array.size, tac->loc);
        return;
    }

    AsmOperand *src = codegen_expression(state, tac->load.src);
    AsmOperand *dst = codegen_expression(state, tac->load.dst);

    codegen_push_instr(state, asm_mov(src, aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
    codegen_push_instr(state, asm_mov(aoper_memory(REG_RDX, 0), dst, type, tac->loc));
}

//
// Generate code for a store operation.
//
static void codegen_store(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_STORE);

    AsmType *type = codegen_tac_to_asmtype(state, tac->store.src);

    if (type->tag == AT_BYTEARRAY && tac->load.src->tag == TAC_VAR && tac->load.dst->tag == TAC_VAR) {
        codegen_store_bytes(state, tac->load.src->var.name, tac->load.dst->var.name, type->array.size, tac->loc);
        return;
    }

    AsmOperand *src = codegen_expression(state, tac->store.src);
    AsmOperand *dst = codegen_expression(state, tac->store.dst);

    codegen_push_instr(state, asm_mov(dst, aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
    codegen_push_instr(state, asm_mov(src, aoper_memory(REG_RDX, 0), type, tac->loc));
}

//
// Generate code for pointer addition.
//
static void codegen_add_ptr(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_ADDPTR);

    AsmOperand *ptr = codegen_expression(state, tac->add_ptr.ptr);
    AsmOperand *dst = codegen_expression(state, tac->add_ptr.dst);

    int scale = tac->add_ptr.scale;

    if (tac->add_ptr.index->tag == TAC_CONST)
    {
        Const cn = tac->add_ptr.index->constant;

        long value;

        if (cn.tag == CON_INTEGRAL)
        {
            if (cn.intval.size == CIS_LONG)
            {
                value = cn.intval.sign == CIS_SIGNED ? (long)cn.intval.value : cn.intval.value;
            }
            else
            {
                value = cn.intval.sign == CIS_SIGNED ? (int)cn.intval.value : (unsigned)cn.intval.value;
            }
        }
        else
        {
            value = cn.floatval;
        }

        value *= scale;

        codegen_push_instr(state, asm_mov(ptr, aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_lea(aoper_memory(REG_RDX, value), dst, tac->loc));
        return;
    }

    AsmOperand *index = codegen_expression(state, tac->add_ptr.index);

    if (scale == 1 || scale == 2 || scale == 4 || scale == 8)
    {
        codegen_push_instr(state, asm_mov(ptr, aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_mov(index, aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
        codegen_push_instr(state, asm_lea(aoper_indexed(REG_RDX, REG_RAX, scale), dst, tac->loc));

        return;
    }

    codegen_push_instr(state, asm_mov(ptr, aoper_reg(REG_RDX), asmtype_quad(), tac->loc));
    codegen_push_instr(state, asm_mov(index, aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
    codegen_push_instr(state, asm_binary(BOP_MULTIPLY, aoper_imm(scale), aoper_reg(REG_RAX), asmtype_quad(), tac->loc));
    codegen_push_instr(state, asm_lea(aoper_indexed(REG_RDX, REG_RAX, 1), dst, tac->loc));
}

//
// Generate code for copy into aggregate
//
static void codegen_copy_to_offset(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_COPY_TO_OFFSET);

    TacCopyToOffset *copy = &tac->copy_to_offset;
    AsmOperand *src = codegen_expression(state, copy->src);
    AsmType *at = codegen_tac_to_asmtype(state, copy->src);

    if (at->tag == AT_BYTEARRAY) {
        codegen_copy_bytes(state, src, aoper_pseudomem(copy->dst, copy->offset), at->array.size, tac->loc);
    } else {
        codegen_push_instr(state, asm_mov(src, aoper_pseudomem(copy->dst, copy->offset), at, tac->loc));
    }
}

//
// Generate code for copy from an aggregate
//
static void codegen_copy_from_offset(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_COPY_FROM_OFFSET);

    TacCopyFromOffset *copy = &tac->copy_from_offset;
    AsmOperand *dst = codegen_expression(state, copy->dst);
    AsmType *at = codegen_tac_to_asmtype(state, copy->dst);

    if (at->tag == AT_BYTEARRAY) {
        codegen_copy_bytes(state, aoper_pseudomem(copy->src, copy->offset), dst, at->array.size, tac->loc);
    } else {
        codegen_push_instr(state, asm_mov(aoper_pseudomem(copy->src, copy->offset), dst, at, tac->loc));
    }
}

//
// Generate code for a single instruction.
//
static void codegen_single(CodegenState *state, TacNode *tac)
{
    switch (tac->tag)
    {
    case TAC_UNARY:
        codegen_unary(state, tac);
        break;
    case TAC_BINARY:
        codegen_binary(state, tac);
        break;
    case TAC_RETURN:
        codegen_return(state, tac);
        break;
    case TAC_JUMP:
        codegen_jump(state, tac);
        break;
    case TAC_JUMP_ZERO:
        codegen_jump_zero(state, tac);
        break;
    case TAC_JUMP_NOT_ZERO:
        codegen_jump_not_zero(state, tac);
        break;
    case TAC_COPY:
        codegen_copy(state, tac);
        break;
    case TAC_LABEL:
        codegen_label(state, tac);
        break;
    case TAC_FUNCTION_CALL:
        codegen_function_call(state, tac);
        break;

    case TAC_STATIC_VAR:
        codegen_static_var(state, tac);
        break;
    case TAC_STATIC_CONST:
        codegen_static_const(state, tac);
        break;
    case TAC_SIGN_EXTEND:
        codegen_sign_extend(state, tac);
        break;
    case TAC_ZERO_EXTEND:
        codegen_zero_extend(state, tac);
        break;
    case TAC_TRUNCATE:
        codegen_truncate(state, tac);
        break;
    case TAC_DOUBLE_TO_INT:
        codegen_dbl_to_int(state, tac);
        break;
    case TAC_DOUBLE_TO_UINT:
        codegen_dbl_to_uint(state, tac);
        break;
    case TAC_INT_TO_DOUBLE:
        codegen_int_to_dbl(state, tac);
        break;
    case TAC_UINT_TO_DOUBLE:
        codegen_uint_to_dbl(state, tac);
        break;

    case TAC_GET_ADDRESS:
        codegen_get_address(state, tac);
        break;
    case TAC_LOAD:
        codegen_load(state, tac);
        break;
    case TAC_STORE:
        codegen_store(state, tac);
        break;

    case TAC_ADDPTR:
        codegen_add_ptr(state, tac);
        break;

    case TAC_COPY_TO_OFFSET:
        codegen_copy_to_offset(state, tac);
        break;

    case TAC_COPY_FROM_OFFSET:
        codegen_copy_from_offset(state, tac);
        break;

    case TAC_PROGRAM:
        break;
    case TAC_CONST:
        break;
    case TAC_VAR:
        break;
    case TAC_FUNCDEF:
        break;
    }
}

//
// Generate code for a function.
//
static void codegen_funcdef(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_FUNCDEF);
    TacFuncDef *func = &tac->funcdef;

    CodegenState funcstate = nested_state(state);
    list_clear(&funcstate.code);

    Symbol *sym = stab_lookup(state->stab, func->name);
    ICE_ASSERT(sym->type->tag == TT_FUNC);
    bool return_in_memory = check_return_in_memory(state->typetab, sym->type->func.ret);

    List ints;
    List floats;
    List stack;

    codegen_classify_parameters(state, &func->parms, &ints, &floats, &stack, return_in_memory);

    //
    // Move parameters into local pseudoregisters.
    //
    int reg_index = 0;
    if (return_in_memory) {
        codegen_push_instr(
            &funcstate,
            asm_mov(
                aoper_reg(REG_RDI),
                aoper_memory(REG_RBP, -8),
                asmtype_quad(),
                tac->loc
            )
        );
        reg_index = 1;
    }

    //
    // Integer register based parameters.
    //
    ListNode *curr = ints.head;
    ListNode *next = NULL;

    for (int i = reg_index; curr; i++, curr = next)
    {
        next = curr->next;
        TypedOperand *to = CONTAINER_OF(curr, TypedOperand, list);
        Register reg = int_arg_regs[i];

        if (to->type->tag == AT_BYTEARRAY) {
            codegen_copy_bytes_from_reg(
                state,
                reg,
                to->oper,
                to->type->array.size,
                tac->loc
            );
        } else {
            codegen_push_instr(
                &funcstate,
                asm_mov(
                    aoper_reg(reg),
                    to->oper,
                    to->type,
                    tac->loc));
        }
        safe_free(to);
    }

    //
    // Float register based parameters.
    //
    curr = floats.head;
    next = NULL;

    for (int i = 0; curr; i++, curr = next)
    {
        next = curr->next;
        TypedOperand *to = CONTAINER_OF(curr, TypedOperand, list);

        codegen_push_instr(
            &funcstate,
            asm_mov(
                aoper_reg(float_arg_regs[i]),
                to->oper,
                to->type,
                tac->loc));
        safe_free(to);
    }

    //
    // Stack based parameters.
    //
    int offset = 16;
    curr = stack.head;
    next = NULL;
    for (int i = 0; curr; i++, curr = next)
    {
        next = curr->next;
        TypedOperand *to = CONTAINER_OF(curr, TypedOperand, list);

        if (to->type->tag == AT_BYTEARRAY) {
            codegen_copy_bytes(
                state,
                aoper_memory(REG_RBP, offset),
                to->oper,
                to->type->array.size,
                tac->loc
            );
        } else {
            codegen_push_instr(
                &funcstate,
                asm_mov(
                    aoper_memory(REG_RBP, offset),
                    to->oper,
                    to->type,
                    tac->loc));
        }
        offset += 8;
        safe_free(to);
    }

    //
    // Statements.
    //
    for (ListNode *curr = tac->funcdef.body.head; curr; curr = curr->next)
    {
        TacNode *stmt = CONTAINER_OF(curr, TacNode, list);
        codegen_single(&funcstate, stmt);
    }

    codegen_push_instr(state, asm_func(tac->funcdef.name, funcstate.code, func->global, tac->loc));
}

//
// Generate code from the AST.
//
AsmNode *codegen(TacNode *tac, SymbolTable *stab, TypeTable *typetab, BackEndSymbolTable *bstab)
{
    ICE_ASSERT(tac->tag == TAC_PROGRAM);

    List statics;
    list_clear(&statics);

    CodegenState state;
    list_clear(&state.code);
    state.statics = &statics;
    state.stab = stab;
    state.typetab = typetab;
    state.bstab = bstab;
    state.neg_zero = NULL;
    state.dbl_to_uint_ub = NULL;

    for (ListNode *curr = tac->prog.decls.head; curr; curr = curr->next)
    {
        TacNode *decl = CONTAINER_OF(curr, TacNode, list);

        switch (decl->tag)
        {
        case TAC_FUNCDEF:
            codegen_funcdef(&state, decl);
            break;
        case TAC_STATIC_VAR:
            codegen_static_var(&state, decl);
            break;
        case TAC_STATIC_CONST:
            codegen_static_const(&state, decl);
            break;

        default:
            ICE_ASSERT(((void)"unexpected tag for top-level TAC node", false));
        }
    }

    list_append(&state.code, state.statics->head);

    AsmNode *prog = asm_prog(state.code, tac->loc);

    safe_free(state.dbl_to_uint_ub);
    safe_free(state.neg_zero);

    return prog;
}

//
// Convert a function symbol to a back end symbol.
//
static void codegen_func_sym_to_backsym(TypeTable *typetab, Type *type, SymFunction *func, BackEndSymbol *bsym)
{
    bsym->tag = BST_FUNCTION;
    bsym->func.is_defined = func->defined;
    bsym->func.return_on_stack = false;
    
    if (type_complete(typetab, type->func.ret)) {
        bsym->func.return_on_stack = check_return_in_memory(typetab, type->func.ret);
    }
}

//
// Convert a static variable symbol to a back end symbol.
//
static void codegen_static_sym_to_backsym(Symbol *sym, BackEndSymbol *bsym, TypeTable *typetab)
{
    ICE_ASSERT(sym->tag == ST_STATIC_VAR);

    bsym->tag = BST_OBJECT;
    bsym->object.type = codegen_type_to_asmtype(typetab, sym->type);
    bsym->object.is_static = true;
}

//
// Convert a local variable symbol to a back end symbol.
//
static void codegen_local_sym_to_backsym(Symbol *sym, BackEndSymbol *bsym, TypeTable *typetab)
{
    ICE_ASSERT(sym->tag == ST_LOCAL_VAR);

    bsym->tag = BST_OBJECT;
    bsym->object.type = codegen_type_to_asmtype(typetab, sym->type);
    bsym->object.is_static = false;
}

//
// Convert a static constant to a back end symbol.
//
static void codegen_static_const_to_backsym(Symbol *sym, BackEndSymbol *bsym, TypeTable *typetab)
{
    bsym->tag = BST_OBJECT;
    bsym->object.type = codegen_type_to_asmtype(typetab, sym->type);
    bsym->object.is_literal = true;
    bsym->object.is_static = true;
}

//
// Allocate a back end symbol table an populate it from the front end
// symbol table.
//
void codegen_sym_to_backsym(SymbolTable *stab, BackEndSymbolTable *bstab, TypeTable *typetab)
{
    HashIterator iter;

    for (HashNode *curr = hashtab_first(stab->hashtab, &iter); curr; curr = hashtab_next(&iter))
    {
        Symbol *sym = CONTAINER_OF(curr, Symbol, hash);
        //
        // We get temporaries of type void due to function calls with a return type of void.
        // These temporaries are never actually used and should not make it into the
        // back end symbol table.
        //
        if (sym->type->tag == TT_VOID) {
            continue;
        }

        BackEndSymbol *bsym = bstab_lookup(bstab, curr->key);

        switch (sym->tag) {
        case ST_FUNCTION:
            codegen_func_sym_to_backsym(typetab, sym->type, &sym->func, bsym);
            break;
        case ST_STATIC_VAR:
            codegen_static_sym_to_backsym(sym, bsym, typetab);
            break;
        case ST_LOCAL_VAR:
            codegen_local_sym_to_backsym(sym, bsym, typetab);
            break;
        case ST_CONSTANT:
            codegen_static_const_to_backsym(sym, bsym, typetab);
            break;
        }
    }
}
