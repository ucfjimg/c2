cmake_minimum_required(VERSION 3.5)
project(compiler)

add_executable(cc
    cc.c

    allocvars.c
    asm-ast.c
    ast.c
    bitmath.c
    backsym.c
    codegen.c
    constant.c
    emitcode.c
    errors.c
    fileline.c
    fixoperands.c
    goto.c
    hashtab.c
    ice.c
    lexer.c
    list.c
    looplabel.c
    mempool.c
    operators.c
    parser.c
    resolve.c
    safemem.c
    strbuilder.c
    strutil.c
    switch.c
    symtab.c
    tacgen.c
    tacnode.c
    target.c
    temporary.c
    token.c
    type.c
    typetab.c
    typecheck.c
)

target_compile_options(cc PRIVATE -Wall -Werror)
