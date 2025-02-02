#include "fileline.h"

#include "safemem.h"

//
// Format a file and line number for debug or error message output.
// Returns an allocated string which must be free'd.
//
char *fileline_describe(FileLine *fl)
{
    if (fl->fname == NULL) {
        return saprintf("<invalid-location>");
    }
    return saprintf("%s:%d", fl->fname, fl->line);
}
