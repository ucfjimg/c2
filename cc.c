#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocvars.h"
#include "asm-ast.h"
#include "backsym.h"
#include "codegen.h"
#include "emitcode.h"
#include "errors.h"
#include "fixoperands.h"
#include "goto.h"
#include "list.h"
#include "looplabel.h"
#include "lexer.h"
#include "parser.h"
#include "resolve.h"
#include "safemem.h"
#include "strbuilder.h"
#include "symtab.h"
#include "switch.h"
#include "tacgen.h"
#include "token.h"
#include "typecheck.h"

typedef enum {
    STAGE_LEX = 256,
    STAGE_PARSE,
    STAGE_CODEGEN,
    STAGE_TACKY,
    STAGE_ALL,

    STAGE_VALIDATE,
    STAGE_VALIDATE_RESOLVE,
    STAGE_VALIDATE_GOTO,
    STAGE_VALIDATE_LOOPS,
    STAGE_VALIDATE_SWITCH,
    STAGE_VALIDATE_TYPECHECK,
    STAGE_VALIDATE_LAST = STAGE_VALIDATE_TYPECHECK,

    STAGE_INVALID,
} Stage;

static bool is_validate_stage(Stage stage)
{
    return stage >= STAGE_VALIDATE && stage <= STAGE_VALIDATE_LAST;
}

typedef enum {
    OPT_KEEP = 512,
    OPT_LINENOS,
    OPT_PRINT_STAB,
} Options;

typedef struct {
    ListNode list;
    char *path;
} PathNode;

typedef struct {
    bool compile_only;                  // if set, don't link, just produce .o file
    bool line_nos;                      // if set, print line number information
    bool keep;                          // if set, don't remove any produced files, even on error
    bool verbose;                       // echo verbose information while compiling and linking
    bool print_stab;                    // if set, print the symbol table
    List includes;                      // list of <PathNode>; -I options from command line
    List non_c_source;                  // list of <PathNode>; non .c source, object, or libraries 
    List libs;                          // list of <PathNode>; names of -l arguments
    char *srcfile;                      // name of source file
    char *prefile;                      // computed name of preprocessed file (.i)
    char *asmfile;                      // computed name of asm file (.s)
    char *objfile;                      // computed name of object file (.o)
    char *binfile;                      // the executable file
    Stage stage;                        // the last pass to run
} Args;

static struct option long_opts[] = {
    { "lex",        no_argument, 0, STAGE_LEX },
    { "parse",      no_argument, 0, STAGE_PARSE },
    { "validate",   optional_argument, 0, STAGE_VALIDATE },
    { "codegen",    no_argument, 0, STAGE_CODEGEN },
    { "tacky",      no_argument, 0, STAGE_TACKY },
    { "keep",       no_argument, 0, OPT_KEEP },
    { "line-nos",   no_argument, 0, OPT_LINENOS },
    { "stab",       no_argument, 0, OPT_PRINT_STAB },
    { 0, 0, 0, 0},    
};

typedef struct {
    char *name;                         // name of substage
    Stage stage;                        // and stage to set
} Substage;

static Substage validate_substages[] = {
    { "resolve",    STAGE_VALIDATE_RESOLVE },
    { "goto",       STAGE_VALIDATE_GOTO },
    { "loops",      STAGE_VALIDATE_LOOPS },
    { "switch",     STAGE_VALIDATE_SWITCH },
    { "typecheck",  STAGE_VALIDATE_TYPECHECK },
    { NULL,         0 },
};

//
// Print usage and exit
//
static void usage(void)
{
    fprintf(stderr, "cc: [-cv] [-Iinclude] [-llib] [--lex | --parse | --validate | --codegen | --tacky]\n    [--keep] [--stab] [--line-no] srcfile [non-c-files]\n");
    exit(1);
}

//
// Return the extension of the given filename, or a pointer to 
// the end of the string if there is no extension. The extension
// is returned including the preceding '.'.
//
// The returned pointer is not allocated, but points into `fname`.
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
// Append an include path to the includes list.
//
static void args_append_include_path(Args *args, char *path)
{
    PathNode *node = safe_malloc(sizeof(PathNode));
    node->path = safe_strdup(path);
    list_push_back(&args->includes, &node->list);
}

//
// Append a non-C file to the non-c files list.
//
static void args_append_non_c_file(Args *args, char *file)
{
    PathNode *node = safe_malloc(sizeof(PathNode));
    node->path = safe_strdup(file);
    list_push_back(&args->non_c_source, &node->list);
}

//
// Append a library name to the libraries list.
//
static void args_append_lib(Args *args, char *lib)
{
    PathNode *node = safe_malloc(sizeof(PathNode));
    node->path = safe_strdup(lib);
    list_push_back(&args->libs, &node->list);
}

//
// Loop up a substage from an option and a substage table. Returns
// STAGE_INVALID if the option was not found in the table.
//
static Stage lookup_substage(Substage *table, char *option)
{
    for (int i = 0; table[i].name; i++) {
        if (strcmp(table[i].name, option) == 0) {
            return table[i].stage;
        }
    }

    return STAGE_INVALID;
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

    while ((flag = getopt_long(argc, argv, "cI:vl:", long_opts, NULL)) != -1) {
        switch (flag) {
            case 'c':
                args->compile_only = true;
                break;

            case 'I':
                args_append_include_path(args, optarg);
                break;

            case 'l':
                args_append_lib(args, optarg);
                break;

            case 'v':
                args->verbose = true;
                break;

            case OPT_KEEP:
                args->keep = true;
                break;

            case OPT_LINENOS:
                args->line_nos = true;
                break;

            case OPT_PRINT_STAB:
                args->print_stab = true;
                break;

            case STAGE_LEX:
            case STAGE_PARSE:
            case STAGE_VALIDATE:
            case STAGE_CODEGEN:
            case STAGE_TACKY:
                if (args->stage != STAGE_ALL) {
                    fprintf(stderr, "--stage may only be specified once.\n");
                    usage();
                }

                if (flag == STAGE_VALIDATE && optarg) {
                    flag = lookup_substage(validate_substages, optarg);
                    if (flag == STAGE_INVALID) {
                        fprintf(stderr, "invalid substage `%s` for --validate.\n", optarg);
                        usage();
                    }
                }

                args->stage = flag;
                break;

            default:
                usage();
        }
    }

    //
    // Pick up list of files to process. Compile .c files and let
    // gcc/linker handle assembly, object, and library files.
    //
    static char *valid_non_c_exts[] = {
        ".s",
        ".o",
        ".a",
        NULL
    };

    //
    // For now, require exactly one .c file.
    //
    for (int i = optind; i < argc; i++) {
        char *ext = get_ext(argv[i]);
        if (strcmp(ext, ".c") == 0) {
            if (args->srcfile) {
                fprintf(stderr, "only one .c file at a time.\n");
                usage();
            }
            args->srcfile = safe_strdup(argv[i]);
            continue;
        }

        int extidx = 0;
        for (; valid_non_c_exts[extidx]; extidx++) {
            if (strcmp(ext, valid_non_c_exts[extidx]) == 0) {
                break;
            }
        }

        if (valid_non_c_exts[extidx] == NULL) {
            fprintf(stderr, "file `%s` has unsupported extension.\n", argv[i]);
            usage();
        }  


        args_append_non_c_file(args, argv[i]);  
    }

    if (args->srcfile == NULL) {
        fprintf(stderr, "missing source file.\n");
        usage();
    }

    if (!has_extension(args->srcfile, ".c")) {
        fprintf(stderr, "input must be a c file.\n");
        usage();
    }
    
    args->prefile = replace_extension(args->srcfile, ".i");
    args->asmfile = replace_extension(args->srcfile, ".s");
    args->objfile = replace_extension(args->srcfile, ".o");
    args->binfile = replace_extension(args->srcfile, "");
}

//
// Free a list of PathNode's.
//
static void free_path_node_list(List *list)
{
    for (ListNode *curr = list->head; curr; ) {
        ListNode *next = curr->next;
        PathNode *node = CONTAINER_OF(curr, PathNode, list);
        safe_free(node->path);
        curr = next;
    }

    list_clear(list);
}

//
// Free command line arguments
//
static void free_args(Args *args)
{
    free_path_node_list(&args->includes);
    free_path_node_list(&args->non_c_source);
    free_path_node_list(&args->libs);
    safe_free(args->srcfile);
    safe_free(args->prefile);
    safe_free(args->asmfile);
    safe_free(args->objfile);
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
// Run an external command and return the status. Echo the 
// command being run if verbose is enabled.
//
static int run(Args *args, char *cmd)
{
    int status = system(cmd);
    if (args->verbose) {
        printf("[%d] %s\n", status, cmd);
    }
    return status;
}



//
// Preprocess the source file; return on success or an error code on
// failure.
//
static int preprocess(Args *args)
{
    StrBuilder *cmd = stb_alloc();

    stb_printf(cmd, "gcc -E");

    for (ListNode *curr = args->includes.head; curr; curr = curr->next) {
        PathNode *path = CONTAINER_OF(curr, PathNode, list);
        stb_printf(cmd, " -I%s", path->path);
    }

    stb_printf(cmd, " %s -o %s", args->srcfile, args->prefile);
    int status = run(args, cmd->str);
    stb_free(cmd);

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
    AstProgram *ast = NULL;
    AsmNode *asmcode = NULL;
    TacNode *taccode = NULL;
    SymbolTable *stab = NULL;
    BackEndSymbolTable *bstab = NULL;
    FILE *asmfile;

    Lexer *lex = lexer_open(args->prefile);
    if (!lex) {
        return 1;
    }

    //
    // Parse.
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
    // Semantic passes.
    //
    ast_resolve(ast);
    if (args->stage == STAGE_VALIDATE_RESOLVE) goto semantic_done;
    ast_validate_goto(ast);
    if (args->stage == STAGE_VALIDATE_GOTO) goto semantic_done;
    ast_label_loops(ast);
    if (args->stage == STAGE_VALIDATE_LOOPS) goto semantic_done;
    ast_validate_switch(ast);
    if (args->stage == STAGE_VALIDATE_SWITCH) goto semantic_done;

    stab = stab_alloc();
    ast_typecheck(ast, stab);

semantic_done:
    if (err_has_errors()) {
        status = 1;
    }

    if (is_validate_stage(args->stage)) {
        ast_print(ast, args->line_nos);
        goto done;
    }

    if (status) {
        goto done;
    }

    //
    // TAC generation.
    //
    taccode = tcg_gen(ast, stab);
    if (args->stage == STAGE_TACKY) {
        tac_print(taccode, args->line_nos);
        goto done;
    }

    //
    // Code generation and assembly transformation passes.
    //
    bstab = bstab_alloc();

    asmcode = codegen(taccode, stab, bstab);
    ast_free_program(ast);
    ast = NULL;

    codegen_sym_to_backsym(stab, bstab);

    asm_allocate_vars(asmcode, bstab);
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

    emitcode(asmfile, asmcode, bstab);

    fclose(asmfile);

done:
    if (stab && args->print_stab) {
        printf("\n=== symbol table ===\n");
        stab_print(stab);
        if (bstab) {
            printf("\n=== back end symbol table ===\n");
            bstab_print(bstab);
        }
    }
    bstab_free(bstab);
    stab_free(stab);
    tac_free(taccode);
    asm_free(asmcode);
    // TODO fix this - current scheme leads to duplicate free's
    //  ast_free_program(ast);
    lexer_close(lex);
    return status;
}

//
// Use gcc to assemble the file.
//
static int assemble(Args *args)
{
    char *cmd = saprintf("gcc -c -o %s %s", args->objfile, args->asmfile);
    int status = run(args, cmd);
    safe_free(cmd);

    return status == 0 ? 0 : 1;
}

//
// Use gcc to link the final program.
//
static int link_program(Args *args)
{
    StrBuilder *cmd = stb_alloc();

    stb_printf(cmd, "gcc");

    for (ListNode *curr = args->includes.head; curr; curr = curr->next) {
        PathNode *path = CONTAINER_OF(curr, PathNode, list);
        stb_printf(cmd, " -I%s", path->path);
    }

    stb_printf(cmd, " -o %s %s", args->binfile, args->asmfile);

    for (ListNode *curr = args->non_c_source.head; curr; curr = curr->next) {
        PathNode *file = CONTAINER_OF(curr, PathNode, list);
        stb_printf(cmd, " %s", file->path);
    }

    for (ListNode *curr = args->libs.head; curr; curr = curr->next) {
        PathNode *lib = CONTAINER_OF(curr, PathNode, list);
        stb_printf(cmd, " -l%s", lib->path);
    }

    int status = run(args, cmd->str);
    stb_free(cmd);

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
    
    if (err_has_errors()) {
        status = 1;
    }

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

    if (args.stage == STAGE_ALL) {
        if (args.compile_only) {
            status = assemble(&args);
        } else {
            status = link_program(&args);
        }
    }

done:
    cleanup(&args);
    free_args(&args);

    return status;
}
