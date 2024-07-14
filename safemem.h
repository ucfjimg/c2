#ifndef _SAFEMEM_H_
#define _SAFEMEM_H_

#include <string.h>

extern void *safe_malloc(size_t len);
extern void *safe_realloc(void *p, size_t len);
extern char *safe_strdup(char *str);
extern void safe_free(void *p);
extern char *saprintf(char *fmt, ...);

#endif


