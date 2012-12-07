#include "readlines.h"

struct RL {
    int fd;
    char *buf;
    size_t max_size;
}

RL* rl_open(int fd, size_t max_size) {
    RL *rl = (RL *) malloc(sizeof RL);
    if (rl == NULL) {
        return NULL;
    }
    rl->buf = (char *) malloc(max_size);
    if (rl->buf == NULL) {
        return NULL;
    }
    rl->max_size = max_size;
    return rl;
}

size_t rl_max_size(RL *rl) {
    return rl->max_size;
}
