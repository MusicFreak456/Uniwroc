#include <stdio.h>

extern void *malloc(size_t size);
extern void free(void *ptr);
extern void *realloc(void *ptr, size_t size);
extern void *calloc(size_t nmemb, size_t size);

extern int mm_init(void);

/* This is largely for debugging */
extern void mm_checkheap(int verbose);
