project(compiler)
cmake_minimum_required(VERSION 3.2)

add_executable(cc
    cc.c

    asm-ast.c
    ast.c
    codegen.c
    errors.c
    fileline.c
    lexer.c
    list.c
    parser.c
    safemem.c
    strbuilder.c
    strutil.c
    token.c
)

