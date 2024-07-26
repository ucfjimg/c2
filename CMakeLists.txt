cmake_minimum_required(VERSION 3.5)
project(compiler)

add_executable(cc
    cc.c

    allocvars.c
    asm-ast.c
    ast.c
    codegen.c
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
    operators.c
    parser.c
    resolve.c
    safemem.c
    strbuilder.c
    symtab.c
    switch.c
    tacgen.c
    tacnode.c
    token.c
    type.c
    typecheck.c
)

target_compile_options(cc PRIVATE -Wall -Werror)