#include "codegen.h"

#include "asm-ast.h"
#include "ice.h"
#include "list.h"
#include "symtab.h"

typedef struct {
    List code;
    SymbolTable *stab;
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
    if (tac->tag == TAC_CONST) {
        return const_unsigned(&tac->constant);
    }
    
    if (tac->tag == TAC_VAR) {
        Symbol *sym = stab_lookup(state->stab, tac->var.name);
        return type_unsigned(sym->type);
    }

    ICE_ASSERT(((void)"invalid TAC tag in codegen_operand_unsigned", false));
    return false;
}

//
// Push an assembly instruction onto a code list.
//
static void codegen_push_instr(CodegenState *state, AsmNode *instr)
{
    list_push_back(&state->code, &instr->list);
}

static AsmOperand *codegen_expression(CodegenState *state, TacNode *tac);

//
// Convert a type system type to the proper assembly type.
//
static AsmType *codegen_type_to_asmtype(Type *type)
{
    ICE_ASSERT(type != NULL);

    switch (type->tag) {
        case TT_INT:        return asmtype_long();
        case TT_UINT:       return asmtype_long();
        case TT_LONG:       return asmtype_quad();
        case TT_ULONG:      return asmtype_quad();
        case TT_DOUBLE:     ICE_NYI("codegen_type_to_asmtype::TT_DOUBLE");
        case TT_FUNC:       ICE_ASSERT(((void)"function symbol found in codegen_type_to_asmtype.", false));
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
    if (tac->tag == TAC_CONST) {
        switch (tac->constant.tag) {
            case CON_INTEGRAL:
                if (tac->constant.intval.size == CIS_LONG) {
                    return asmtype_quad();
                } else {
                    return asmtype_long();
                }
                break;

            case CON_FLOAT: ICE_NYI("codegen_type_to_asmtype::CON_FLOAT");
        }
    }

    //
    // If it's a variable, look it up in the symbol 
    // table to see what size it is.
    //
    if (tac->tag == TAC_VAR) {
        Symbol *sym = stab_lookup(state->stab, tac->var.name);
        return codegen_type_to_asmtype(sym->type);
    }

    ICE_ASSERT(((void)"TAC node in codegen_tac_to_asmtype was not an operand.", false));
    return asmtype_long();
}

//
// Return the alignment for a static variable.
//
static int codegen_align(Const *init)
{
    if (init->tag == CON_FLOAT) {
        return 8;
    }

    if (init->tag == CON_INTEGRAL) {
        switch (init->intval.size) {
            case CIS_INT:       return 4;
            case CIS_LONG:      return 8;
        }
    }

    return 1;
}

//
// Generate code for a constant.
//
static AsmOperand *codegen_constant(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_CONST);
    return aoper_imm(tac->constant.intval.value);
}

//
// Generate code for a variable reference.
//
static AsmOperand *codegen_var(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_VAR);
    return aoper_pseudoreg(tac->var.name);
}

//
// Generate code for an expression. Code to generate the value
// will be appended to state->code, and an unreferenced operand
// node containing the result location will be returned.
//
static AsmOperand *codegen_expression(CodegenState *state, TacNode *tac)
{
    switch (tac->tag) {
        case TAC_CONST:     return codegen_constant(state, tac);
        case TAC_VAR:       return codegen_var(state, tac);

        default:
            ICE_ASSERT(("invalid TAC node in codegen_expression"));
    }

    //
    // never reached
    //
    return NULL;
}

//
// Generate code for a unary operator.
//
static void codegen_unary(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_UNARY);

    AsmOperand *src = codegen_expression(state, tac->unary.src);
    AsmOperand *dst = codegen_expression(state, tac->unary.dst);


    if (tac->unary.op == UOP_LOGNOT) {
        AsmType *srctype = codegen_tac_to_asmtype(state, tac->unary.src);
        AsmType *dsttype = codegen_tac_to_asmtype(state, tac->unary.dst);

        codegen_push_instr(state, asm_cmp(aoper_imm(0), src, srctype, tac->loc));
        codegen_push_instr(state, asm_mov(aoper_imm(0), dst, dsttype, tac->loc));
        codegen_push_instr(state, asm_setcc(aoper_clone(dst), ACC_E, tac->loc));

        return;
    }

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

    if (is_unsigned) {
        switch (tac->binary.op) {
            case BOP_EQUALITY:      cc = ACC_E; break;
            case BOP_NOTEQUAL:      cc = ACC_NE; break;
            case BOP_LESSTHAN:      cc = ACC_B; break;
            case BOP_GREATERTHAN:   cc = ACC_A; break;
            case BOP_LESSEQUAL:     cc = ACC_BE; break;
            case BOP_GREATEREQUAL:  cc = ACC_AE; break;
            
            default:
                ICE_ASSERT(((void)"invalid binary op in codegen_relational", false));
        }
    } else {
        switch (tac->binary.op) {
            case BOP_EQUALITY:      cc = ACC_E; break;
            case BOP_NOTEQUAL:      cc = ACC_NE; break;
            case BOP_LESSTHAN:      cc = ACC_L; break;
            case BOP_GREATERTHAN:   cc = ACC_G; break;
            case BOP_LESSEQUAL:     cc = ACC_LE; break;
            case BOP_GREATEREQUAL:  cc = ACC_GE; break;
            
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
// Generate code for a binary operator.
//
static void codegen_binary(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_BINARY);

    if (bop_is_relational(tac->binary.op)) {
        codegen_relational(state, tac);
        return;
    }

    AsmOperand *left = codegen_expression(state, tac->binary.left);
    AsmOperand *right = codegen_expression(state, tac->binary.right);
    AsmOperand *dst = codegen_expression(state, tac->binary.dst);

    if (tac->binary.op == BOP_DIVIDE || tac->binary.op == BOP_MODULO) {
        bool is_unsigned = codegen_operand_unsigned(state, tac->binary.left);
        AsmType *lefttype = codegen_tac_to_asmtype(state, tac->binary.left);

        codegen_push_instr(state, asm_mov(left, aoper_reg(REG_RAX), lefttype, tac->loc));
        if (is_unsigned) {
            lefttype = asmtype_clone(lefttype);
            codegen_push_instr(state, asm_binary(BOP_BITXOR, aoper_reg(REG_RDX), aoper_reg(REG_RDX), lefttype, tac->loc));
            lefttype = asmtype_clone(lefttype);
            codegen_push_instr(state, asm_div(right, lefttype, tac->loc));
        } else {
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

        return;
    }

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

    AsmOperand *retval = codegen_expression(state, tac->ret.val);

    AsmType *rettype = codegen_tac_to_asmtype(state, tac->ret.val);
    codegen_push_instr(state, asm_mov(retval, aoper_reg(REG_RAX), rettype, tac->loc));
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

    codegen_push_instr(state, asm_cmp(aoper_imm(0), cond, condtype, tac->loc));
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

    codegen_push_instr(state, asm_cmp(aoper_imm(0), cond, condtype, tac->loc));
    codegen_push_instr(state, asm_jumpcc(tac->jump_zero.target, ACC_NE, tac->loc));
}

//
// Generate code for a copy instruction.
//
static void codegen_copy(CodegenState *state, TacNode *tac)
{
    ICE_ASSERT(tac->tag == TAC_COPY);

    AsmOperand *src = codegen_expression(state, tac->copy.src);
    AsmOperand *dst = codegen_expression(state, tac->copy.dst);
    AsmType *dsttype = codegen_tac_to_asmtype(state, tac->copy.dst);

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

    int arg_count = list_count(&call->args);
    int args_in_regs = arg_count < int_arg_reg_count ? arg_count : int_arg_reg_count;
    int args_on_stack = arg_count - args_in_regs;

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
    // Load register arguments.
    //
    ListNode *curr = call->args.head;
    for (int i = 0; i < args_in_regs; i++, curr = curr->next) {
        Register arg_reg = int_arg_regs[i];
        TacNode *tac_arg = CONTAINER_OF(curr, TacNode, list);
        AsmOperand *arg = codegen_expression(state, tac_arg);
        AsmType *argtype = codegen_tac_to_asmtype(state, tac_arg);
        codegen_push_instr(state, asm_mov(arg, aoper_reg(arg_reg), argtype, tac->loc));
    }

    //
    // Push stack args.
    //
    list_reverse(&call->args);

    curr = call->args.head;
    for (int i = 0; i < args_on_stack; i++, curr = curr->next) {
        TacNode *tac_arg = CONTAINER_OF(curr, TacNode, list);
        AsmOperand *arg = codegen_expression(state, tac_arg);
        AsmType *argtype = codegen_tac_to_asmtype(state, tac_arg);
        codegen_push_instr(state, asm_mov(arg, aoper_reg(REG_RAX), argtype, tac->loc));
        codegen_push_instr(state, asm_push(aoper_reg(REG_RAX), tac->loc));
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
    // Return value, if any, goes in RAX.
    //
    AsmOperand *result = codegen_expression(state, call->dst);
    AsmType *restype = codegen_tac_to_asmtype(state, call->dst);
    codegen_push_instr(state, asm_mov(aoper_reg(REG_RAX), result, restype, tac->loc));
}

//
// Geneate code for a static variable.
//
static void codegen_static_var(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_STATIC_VAR);

    codegen_push_instr(state, 
        asm_static_var(
            decl->static_var.name, 
            decl->static_var.global, 
            codegen_align(&decl->static_var.init),
            decl->static_var.init, 
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
            codegen_expression(state, decl->sign_extend.src),
            codegen_expression(state, decl->sign_extend.dst),
            decl->loc)
    );
}

//
// Generate code for a zero extend operation.
//
static void codegen_zero_extend(CodegenState *state, TacNode *decl)
{
    ICE_ASSERT(decl->tag == TAC_ZERO_EXTEND);

    codegen_push_instr(state,
        asm_movzx(
            codegen_expression(state, decl->zero_extend.src),
            codegen_expression(state, decl->zero_extend.dst),
            decl->loc)
    );
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
            asmtype_long(),
            decl->loc)
    );
}

//
// Generate code for a single instruction.
//
static void codegen_single(CodegenState *state, TacNode *tac)
{
    switch (tac->tag) {
        case TAC_UNARY:             codegen_unary(state, tac); break;
        case TAC_BINARY:            codegen_binary(state, tac); break;
        case TAC_RETURN:            codegen_return(state, tac); break;
        case TAC_JUMP:              codegen_jump(state, tac); break;
        case TAC_JUMP_ZERO:         codegen_jump_zero(state, tac); break;
        case TAC_JUMP_NOT_ZERO:     codegen_jump_not_zero(state, tac); break;
        case TAC_COPY:              codegen_copy(state, tac); break;
        case TAC_LABEL:             codegen_label(state, tac); break;
        case TAC_FUNCTION_CALL:     codegen_function_call(state, tac); break;

        case TAC_STATIC_VAR:        codegen_static_var(state, tac); break; 
        case TAC_SIGN_EXTEND:       codegen_sign_extend(state, tac); break;
        case TAC_ZERO_EXTEND:       codegen_zero_extend(state, tac); break;
        case TAC_TRUNCATE:          codegen_truncate(state, tac); break;

        case TAC_DOUBLE_TO_INT:     ICE_NYI("codegen_single::double-to-int");
        case TAC_DOUBLE_TO_UINT:    ICE_NYI("codegen_single::double-to-uint");
        case TAC_INT_TO_DOUBLE:     ICE_NYI("codegen_single::int-to-double");
        case TAC_UINT_TO_DOUBLE:    ICE_NYI("codegen_single::uint-to-double");

        case TAC_PROGRAM:           break;
        case TAC_CONST:             break;
        case TAC_VAR:               break;
        case TAC_FUNCDEF:           break;
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
    ICE_ASSERT(sym->type && sym->type->tag == TT_FUNC);

    //
    // Move parameters into local pseudoregisters.
    //
    int param_count = list_count(&func->parms);
    int params_in_regs = param_count > int_arg_reg_count ? int_arg_reg_count : param_count;
    int params_on_stack = param_count - params_in_regs;

    //
    // Register based parameters.
    //
    ListNode *pcurr = func->parms.head;
    ListNode *tcurr = sym->type->func.parms.head; 

    for (
        int i = 0; 
        i < params_in_regs; 
        i++, pcurr = pcurr->next, tcurr = tcurr->next) {
        
        TacFuncParam *param = CONTAINER_OF(pcurr, TacFuncParam, list);
        Type *ptype = CONTAINER_OF(tcurr, TypeFuncParam, list)->parmtype;
        AsmType *at = codegen_type_to_asmtype(ptype);
        codegen_push_instr(
            &funcstate,
            asm_mov(
                aoper_reg(int_arg_regs[i]),
                aoper_pseudoreg(param->name),
                at,
                tac->loc
            )
        );
    }

    //
    // Stack based parameters.
    //
    int offset = 16;
    for (
        int i = 0; 
        i < params_on_stack; 
        i++, pcurr = pcurr->next, tcurr = tcurr->next) {
        TacFuncParam *param = CONTAINER_OF(pcurr, TacFuncParam, list);
        Type *ptype = CONTAINER_OF(tcurr, TypeFuncParam, list)->parmtype;
        AsmType *at = codegen_type_to_asmtype(ptype);
        codegen_push_instr(
            &funcstate,
            asm_mov(
                aoper_stack(offset),
                aoper_pseudoreg(param->name),
                at,
                tac->loc
            )
        );

        offset += 8;
    }

    //
    // Statements.
    //
    for (ListNode *curr = tac->funcdef.body.head; curr; curr = curr->next) {
        TacNode *stmt = CONTAINER_OF(curr, TacNode, list);
        codegen_single(&funcstate, stmt);
    }

    codegen_push_instr(state, asm_func(tac->funcdef.name, funcstate.code, func->global, tac->loc));
}

//
// Generate code from the AST.
//
AsmNode *codegen(TacNode *tac, SymbolTable *stab)
{
    ICE_ASSERT(tac->tag == TAC_PROGRAM);

    CodegenState state;
    list_clear(&state.code);
    state.stab = stab;

    for (ListNode *curr = tac->prog.decls.head; curr; curr = curr->next) {
        TacNode *decl = CONTAINER_OF(curr, TacNode, list);

        switch (decl->tag) {
            case TAC_FUNCDEF: codegen_funcdef(&state, decl); break;
            case TAC_STATIC_VAR: codegen_static_var(&state, decl); break;

            default: ICE_ASSERT(((void)"unexpected tag for top-level TAC node", false)); 
        }
    }

    return asm_prog(state.code, tac->loc);
}

//
// Convert a function symbol to a back end symbol.
//
void codegen_func_sym_to_backsym(SymFunction *func, BackEndSymbol *bsym)
{
    bsym->tag = BST_FUNCTION;
    bsym->func.is_defined = func->defined; 
}

//
// Convert a static variable symbol to a back end symbol.
//
void codegen_static_sym_to_backsym(Symbol *sym, BackEndSymbol *bsym)
{
    ICE_ASSERT(sym->tag == ST_STATIC_VAR);

    bsym->tag = BST_OBJECT;
    bsym->object.type = codegen_type_to_asmtype(sym->type);
    bsym->object.is_static = true;
}

//
// Convert a local variable symbol to a back end symbol.
//
void codegen_local_sym_to_backsym(Symbol *sym, BackEndSymbol *bsym)
{
    ICE_ASSERT(sym->tag == ST_LOCAL_VAR);

    bsym->tag = BST_OBJECT;
    bsym->object.type = codegen_type_to_asmtype(sym->type);
    bsym->object.is_static = false;
}

//
// Allocate a back end symbol table an populate it from the front end
// symbol table.
// 
BackEndSymbolTable *codegen_sym_to_backsym(SymbolTable *stab)
{
    HashIterator iter;

    BackEndSymbolTable *bstab = bstab_alloc();

    for (HashNode *curr = hashtab_first(stab->hashtab, &iter); curr; curr = hashtab_next(&iter)) {
        Symbol *sym = CONTAINER_OF(curr, Symbol, hash);        
        BackEndSymbol *bsym = bstab_lookup(bstab, curr->key);
        
        switch (sym->tag) {
            case ST_FUNCTION:   codegen_func_sym_to_backsym(&sym->func, bsym); break;
            case ST_STATIC_VAR: codegen_static_sym_to_backsym(sym, bsym); break;
            case ST_LOCAL_VAR:  codegen_local_sym_to_backsym(sym, bsym); break;
                break;
        }

    }

    return bstab;
}

