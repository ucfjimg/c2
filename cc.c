#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocvars.h"
#include "asm-ast.h"
#include "codegen.h"
#include "emitcode.h"
#include "errors.h"
#include "fixoperands.h"
#include "lexer.h"
#include "parser.h"
#include "safemem.h"
#include "tacgen.h"
#include "token.h"

typedef enum {
    STAGE_LEX = 256,
    STAGE_PARSE,
    STAGE_CODEGEN,
    STAGE_TACKY,
    STAGE_ALL,
} Stage;

typedef enum {
    OPT_KEEP = 512,
    OPT_LINENOS,
} Options;

typedef struct {
    bool compile_only;                  // if set, don't link, just produce .s file
    bool line_nos;                      // if set, print line number information
    bool keep;                          // if set, don't remove any produced files, even on error
    char *srcfile;                      // name of source file
    char *prefile;                      // computed name of preprocessed file (.i)
    char *asmfile;                      // computed name of asm file (.s)
    char *binfile;                      // the executable file
    Stage stage;                        // the last pass to run
} Args;

static struct option long_opts[] = {
    { "lex",        no_argument, 0, STAGE_LEX },
    { "parse",      no_argument, 0, STAGE_PARSE },
    { "codegen",    no_argument, 0, STAGE_CODEGEN },
    { "tacky",      no_argument, 0, STAGE_TACKY },
    { "keep",       no_argument, 0, OPT_KEEP },
    { "line-nos",   no_argument, 0, OPT_LINENOS },
    { 0, 0, 0, 0},    
};

//
// Print usage and exit
//
static void usage(void)
{
    fprintf(stderr, "cc: [-c] [--lex | --parse | --codegen | --tacky] [--keep] [--line-no] srcfile\n");
    exit(1);
}

//
// Return the extension of the given filename, or a pointer to 
// the end of the string if there is no extension. The extension
// is returned including the preceding '.'.
//
static char *get_ext(char *fname)
{
    int len = strlen(fname);
    int i;

    if (len == 0) {
        return fname;
    }

    for (i = len; i >= 0; i--) {
        if (fname[i] == '\\' || fname[i] == '/') {
            break;
        }

        if (fname[i] == '.') {
            return fname + i;
        }
    }

    return fname + len - 1;
}

//
// Return true if `fname` has extension `ext`. `ext` should 
// include a leading '.'.
// 
static bool has_extension(char *fname, char *ext)
{
    return strcmp(get_ext(fname), ext) == 0;
}

//
// Replace the extension on `fname` with `ext`. `ext` should
// include a leading '.'.
//
// The returned string is malloc()'ed.
//
static char *replace_extension(char *fname, char *ext)
{
    char *extptr = get_ext(fname);

    return saprintf("%.*s%s", (int)(extptr - fname), fname, ext); 
}

//
// Parse command line arguments in `argc` and `argv`, populating
// `args`.
//
// On any error, an error is printed and the process will exit with
// a non-zero status.
//
static void parse_args(int argc, char *argv[], Args *args)
{
    int flag;

    memset(args, 0, sizeof(Args));

    args->stage = STAGE_ALL;

    while ((flag = getopt_long(argc, argv, "c", long_opts, NULL)) != -1) {
        switch (flag) {
            case 'c':
                args->compile_only = true;
                break;

            case OPT_KEEP:
                args->keep = true;
                break;

            case OPT_LINENOS:
                args->line_nos = true;
                break;

            case STAGE_LEX:
            case STAGE_PARSE:
            case STAGE_CODEGEN:
            case STAGE_TACKY:
                if (args->stage != STAGE_ALL) {
                    fprintf(stderr, "--stage may only be specified once.\n");
                    usage();
                }

                args->stage = flag;
                break;

            default:
                usage();
        }
    }

    if (optind + 1 != argc) {
        usage();
    }

    args->srcfile = safe_strdup(argv[optind]);
    
    if (!has_extension(args->srcfile, ".c")) {
        fprintf(stderr, "input must be a c file.\n");
        usage();
    }
    
    args->prefile = replace_extension(args->srcfile, ".i");
    args->asmfile = replace_extension(args->srcfile, ".s");
    args->binfile = replace_extension(args->srcfile, "");
}

//
// Free command line arguments
//
static void free_args(Args *args)
{
    safe_free(args->srcfile);
    safe_free(args->prefile);
    safe_free(args->asmfile);
    safe_free(args->binfile);
}

//
// Clean up temporary files.
//
static void cleanup(Args *args)
{
    if (!args->keep) {
        remove(args->prefile);
        remove(args->asmfile);
    }
}

//
// Preprocess the source file; return on success or an error code on
// failure.
//
static int preprocess(Args *args)
{
    char *cmd = saprintf("gcc -E %s -o %s", args->srcfile, args->prefile);
    int status = system(cmd);
    safe_free(cmd);

    return status == 0 ? 0 : 1;
}

//
// Compile the given preprocessed file, producing an assembly file.
//
// Returns 0 on success or a non-zero status on any error.
//
static int compile(Args *args)
{
    int status = 0;
    AstNode *ast = NULL;
    AsmNode *asmcode = NULL;
    TacNode *taccode = NULL;
    FILE *asmfile;

    Lexer *lex = lexer_open(args->prefile);
    if (!lex) {
        return 1;
    }

    //
    // Parse and AST transformation passes.
    //
    ast = parser_parse(lex);
    if (err_has_errors()) {
        status = 1;
    }

    if (args->stage == STAGE_PARSE) {
        ast_print(ast, args->line_nos);
        goto done;
    }

    if (status) {
        goto done;
    }

    //
    // TAC generation.
    //
    taccode = tcg_gen(ast);
    if (args->stage == STAGE_TACKY) {
        tac_print(taccode, args->line_nos);
        goto done;
    }

    //
    // Code generation and assembly transformation passes.
    //
    asmcode = codegen(taccode);
    ast_free(ast);
    ast = NULL;

    asm_allocate_vars(asmcode);
    asm_fix_operands(asmcode);

    if (args->stage == STAGE_CODEGEN) {
        asm_print(asmcode, args->line_nos);
        goto done;
    }

    //
    // Final assembly source emission.
    //
    asmfile = fopen(args->asmfile, "w");
    if (!asmfile) {
        err_report(EC_ERROR, NULL, "cannot open assembly file `%s`,", args->asmfile);
        status = 1;
        goto done; 
    }

    emitcode(asmfile, asmcode);

    fclose(asmfile);

done:
    tac_free(taccode);
    asm_free(asmcode);
    ast_free(ast);
    lexer_close(lex);
    return status;
}

//
// Use gcc to link the final program.
//
static int link_program(Args *args)
{
    char *cmd = saprintf("gcc -o %s %s", args->binfile, args->asmfile);
    int status = system(cmd);
    safe_free(cmd);

    return status == 0 ? 0 : 1;
}

//
// For debugging, lex the program and dump out the scanned tokens, but do not
// parse.
//
// Returns 0 on success, 1 on failure from lexer.
//
static int lex_pass(Args *args)
{
    int status = 0;
    bool done = false;

    Lexer *lex = lexer_open(args->prefile);
    if (!lex) {
        return 1;
    }

    while (!done) {
        Token tok;
        lexer_token(lex, &tok);

        if (args->line_nos) {
            char *fl = fileline_describe(&tok.loc);
            printf("%s: ", fl);
            safe_free(fl);
        }

        char *desc = token_describe(&tok);
        printf("%s\n", desc);
        safe_free(desc);

        if (tok.type == TOK_ERROR) {
            status = 1;
        }

        done = tok.type == TOK_EOF; 
        token_free(&tok);
    }

    lexer_close(lex);
    return status;
}

int main(int argc, char *argv[])
{
    int status = 0;
    Args args;
        
    parse_args(argc, argv, &args);

    status = preprocess(&args);
    if (status) {
        goto done;
    }

    if (args.stage == STAGE_LEX) {
        status = lex_pass(&args);
        goto done;
    }

    status = compile(&args);
    if (status) {
        goto done;
    }

    if (args.stage == STAGE_ALL && !args.compile_only) {
        status = link_program(&args);
    }

done:
    cleanup(&args);
    free_args(&args);

    return status;
}
