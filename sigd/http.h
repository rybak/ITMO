#ifndef HTTP_H
#define HTTP_H

const size_t BUF_SIZE = 128 * 1024;
const size_t FN_SIZE = 1024;
const size_t MSG_SIZE = 3;
const char OK_MSG[] = "ok\0";
const char ERR_MSG[] = "err ";
void out_ok(int lvl = 0);
void handle_error(char *);

int write_all(int, const char *, int);

int read_all(int, char *, int);

int init_socket(char *, char *, struct addrinfo **);

int init_connect_socket(char *, char *);

int init_listen_socket(char *, char *);

#endif

