#ifndef COMMON_H
#define COMMON_H

#include <signal.h>
#include <stddef.h>

const size_t MAX_LEN = 10;
const size_t TIME_GAP = 15;
const size_t TIME_INTERVAL = 5;
const int SIGCHANGEIP = SIGUSR1;
struct message_t
{
    int ip;
    char name[MAX_LEN];
    int timestamp;
    char student[MAX_LEN];
};

void die(const char *);
void dontdie(const char *);
void make_socket(int &, struct
        sockaddr_in &, uint16_t);
#endif
