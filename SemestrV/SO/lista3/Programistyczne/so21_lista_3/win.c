#include "csapp.h"
#include "sys/ioctl.h"

static void signal_handler(int signo) {
    struct winsize size;

    ioctl(0,TIOCGWINSZ, &size);
    safe_printf("Window resized: %d x %d\n", size.ws_row, size.ws_col);
}

int main(void) {
    Signal(SIGWINCH,signal_handler);
    
    while (1) continue;
    
    return 0;
}