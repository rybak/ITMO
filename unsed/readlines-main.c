#include "readlines.h"
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: readlines-main N\n");
        return 1;
    }
    int n = atoi(argv[1]);
    RL *rl = rl_open(0, n);
    char *buf = malloc(n+1);
    while (1) {
        int len = rl_readline(rl, buf, n);
        if (len == 0) {
            break;
        }
        if (len == -1) {
            perror("The following error occurred: ");
        }
        if (len > 0) {
            int written = 0;
            while (written < len) {
                written += write(1, buf + written, len - written);
            }
        }
    }
    rl_close(rl);
    return 0;
}
