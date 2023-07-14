#include "readlines.h"
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    // printf("%s started\n", argv[0]);
    // fflush(stdout);
    if (argc != 2) {
        printf("Usage: readlines-main N\n");
        return 1;
    }
    int n = atoi(argv[1]);
    RL *rl = rl_open(0, n);
    char *buf = malloc(n + 1);
    while (1) {
        int len = rl_readline(rl, buf, n + 1);
        if (len == 0) {
            break;
        }
        if (len == -1) {
            perror("The following error occurred: ");
            break;
        }
        if (len > 0) {
            int written = 0;
            while (written < len) {
                written += write(1, buf + written, len - written);
            }
        } else {
            printf("\n");
            fflush(stdout);
        }
    }
    rl_close(rl);
    // printf("%s finished\n", argv[0]);
    return 0;
}
