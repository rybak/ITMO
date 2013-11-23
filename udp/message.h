#ifndef MESSAGE_H
#define MESSAGE_H

const size_t MAX_LEN = 20;
const size_t TIME_GAP = 15;
const size_t TIME_INTERVAL = 5;
#define CHAR_TYPE char
struct message_t
{
    int ip;
    CHAR_TYPE name[MAX_LEN];
    long long timestamp;
    CHAR_TYPE student[MAX_LEN];
};



#endif
