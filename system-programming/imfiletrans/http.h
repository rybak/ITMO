#ifndef HTTP_H
#define HTTP_H 1
#define ERROR "Error : "
const size_t BUF_SIZE = 1024;
const size_t MSG_SIZE = 5;
const size_t TOKEN_SIZE = sizeof(int);
const char SEND_MSG[] = "send";
const char RECV_MSG[] = "recv";
void out_ok(int lvl = 0);
int write_all(int, const char *, int);

int read_all(int, char *, int);

int init_socket(char *, char *, struct addrinfo **);

int init_connect_socket(char *, char *);

int init_listen_socket(char *, char *);

#endif

