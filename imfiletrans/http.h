#ifndef HTTP_H
#define HTTP_H 1

const size_t BUF_SIZE = 4096;
const size_t MSG_SIZE = 5;
const size_t TOKEN_SIZE = sizeof(int);
const char SEND_MSG[] = "send";
const char RECV_MSG[] = "recv";

int send_all(int, const char *, int);

int recv_all(int, char *, int);

int init_socket(char *, char *, struct addrinfo **);

int init_connect_socket(char *, char *);

int init_listen_socket(char *, char *);

#endif

