#pragma once

#include "fileline.h"

#include <stdbool.h>

typedef enum ErrorClass {
    EC_FATAL,
    EC_ERROR,
    EC_WARNING,
} ErrorClass;

extern void err_report(ErrorClass ec, FileLine *loc, char *format, ...);
extern bool err_has_errors(void);


