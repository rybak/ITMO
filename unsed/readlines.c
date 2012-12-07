#include "readlines.h"

struct RL {
    int fd;
    char *buf;
    size_t max_size;
    size_t offset;
    size_t length;
}

RL* rl_open(int fd, size_t max_size) {
    RL *rl = (RL *) malloc(sizeof RL);
    if (rl == NULL) {
        return NULL;
    }
    rl->buf = (char *) malloc(max_size);
    if (rl->buf == NULL) {
        free(rl);
        return NULL;
    }
    rl->fd = fd;
    rl->max_size = max_size;
    return rl;
}

size_t rl_max_size(RL *rl) {
    if (rl == NULL)
        return 0;
    return rl->max_size;
}

int rl_close(RL *rl) {
    if (rl == NULL)
        return 0;
    free(rl->buf);
    int fd = rl->fd;
    free(rl);
    return close(fd);
}

int rl_readline(RL *rl, char *buf, size_t buf_size) {

}
