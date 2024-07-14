#pragma once

typedef struct {
    const char *fname;
    int line;
} FileLine;

extern char *fileline_describe(FileLine *fl);
