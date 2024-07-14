project(compiler)
cmake_minimum_required(VERSION 3.2)

add_executable(cc
    cc.c

    errors.c
    fileline.c
    lexer.c
    safemem.c
    strbuilder.c
    token.c
)

