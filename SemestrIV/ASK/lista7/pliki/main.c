#include<stdlib.h>

union elem {
    struct {
        long *p;
        long y;
    } e1;
    
    struct {
        long x;
        union elem *next;
    } e2;
};

union elem* proc(union elem *u){
    union elem *next = u -> e2.next;
    long p_value = *(next -> e1.p);
    u -> e2.x = p_value - next -> e1.y;
    return next;
}
