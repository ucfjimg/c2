project(compiler)
cmake_minimum_required(VERSION 3.2)

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
    hashtab.c
    ice.c
    lexer.c
    list.c
    operators.c
    parser.c
    safemem.c
    strbuilder.c
    strutil.c
    tacgen.c
    tacnode.c
    token.c
)

