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
    rl->fd = fd;
    rl->max_size = max_size;
    return rl;
}

size_t rl_max_size(RL *rl) {
    return rl->max_size;
}

int rl_close(RL *rl) {
    free(rl->buf);
    int res = close(rl->fd);
    free(rl);
    return res;
}

int rl_readline(RL *rl, char *buf, size_t buf_size) {

}
