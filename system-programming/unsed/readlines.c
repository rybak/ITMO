#include "readlines.h"
#include <stdlib.h>
#include <string.h>

typedef struct RL {
    int fd;
    char *buf;
    size_t max_size;
    int length;
} RL;

RL* rl_open(int fd, size_t max_size) {
    RL *rl = (RL *) malloc(sizeof(RL));
    if (rl == NULL) {
        return NULL;
    }
    rl->buf = (char *) malloc(max_size + 1);
    if (rl->buf == NULL) {
        free(rl);
        return NULL;
    }
    rl->fd = fd;
    rl->max_size = max_size;
    rl->length = 0;
    return rl;
}

size_t rl_max_size(RL *rl) {
    if (rl == NULL) {
        return 0;
    }
    return rl->max_size;
}

int rl_close(RL *rl) {
    if (rl == NULL) {
        return 0;
    }
    free(rl->buf);
    int fd = rl->fd;
    free(rl);
    return close(fd);
}

int rl_readline(RL *rl, char *buf, size_t buf_size) {
    if (rl == NULL) {
        return -1;
    }
    size_t size = buf_size;
    if (size > (rl->max_size + 1)) {
        size = rl->max_size + 1;
    }
    int tc = rl->length;
    int nl_pos = -1;
    int j;
    for (j = 0; j < tc; ++j) {
        if (rl->buf[j] == 10) {
            nl_pos = j;
            break;
        }
    }
    while (tc < rl->max_size + 1 && nl_pos == -1) {
        int count = read(rl->fd, rl->buf + tc, rl->max_size + 1 - tc);
        if (count == -1) {
            return -1;
        }
        if (count == 0) {
            return 0;
        }
        int i;
        for (i = 0; i < count; ++i) {
            if (rl->buf[tc + i] == 10) {
                nl_pos = tc + i;
                break;
            }
        }
        tc += count;
    }
    if (nl_pos == -1) {
        rl->length = 0;
        return -3;
    }
    int res = -2;
    if (nl_pos < buf_size) {
        memcpy(buf, rl->buf, nl_pos + 1);
        res = nl_pos + 1;
    }
    size_t i = nl_pos + 1;
    memmove(rl->buf, rl->buf + nl_pos + 1, tc - nl_pos - 1);
//   for (; i < tc; ++i) {
//       rl->buf[i - nl_pos - 1] = rl->buf[i];
//   }
    rl->length = tc - nl_pos - 1;
    return res;
}
