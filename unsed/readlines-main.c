#include "readlines.h"
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

void m(char *s) {
    printf(s);
    fflush(stdin);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        m("Usage: readlines-main N\n");
        return 1;
    }
    m("readlines-main started\n");
    int n = atoi(argv[1]);
    RL *rl = rl_open(0, n);
    char *buf = malloc(n + 1);
    m("Cycle\n");
    while (1) {
        m("\nread...\n");
        int len = rl_readline(rl, buf, n + 1);
        m("\nread : OK\n");
        fflush(stdin);
        if (len == 0) {
            break;
        }
        if (len == -1) {
            perror("The following error occurred: ");
            break;
        }
        if (len > 0) {
            int written = 0;
            printf("len = %d ", len);
            m("\nwriting\n");
            while (written < len) {
                written += write(1, buf + written, len - written);
            }
            m("\nwriting finished\n");
        }
    }
    rl_close(rl);
    m("readlines-main finished\n");
    return 0;
}
