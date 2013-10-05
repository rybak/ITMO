#ifndef COMMON_H
#define COMMON_H

#include <signal.h>
#include <stddef.h>

const size_t MAX_LEN = 20;
const size_t TIME_GAP = 15;
const size_t TIME_INTERVAL = 5;
const int SIGCHANGEIP = SIGUSR1;
#define CHAR_TYPE char
struct message_t
{
    int ip;
    CHAR_TYPE name[MAX_LEN];
    union
    {
        long long timestamp;
        struct tt
        {
            int a;
            int t;
        } s;
    } u;
    CHAR_TYPE student[MAX_LEN];
};

void die(const char *);
void dontdie(const char *);
void make_socket(int &, struct
        sockaddr_in &, uint16_t);
#endif
