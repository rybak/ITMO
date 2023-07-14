#ifndef BIG_BUFFER_H
#define BIG_BUFFER_H
#include <stddef.h>
#include <stdlib.h>
#include <cstring>
#include <unistd.h>
#include <fcntl.h>
#include <iostream>

// const size_t BIG_BUFFER_SIZE = 32 * 1024 * 1024;
const size_t BIG_BUFFER_SIZE = 32;
#define BBERROR "\t\tbig buffer error : "
struct big_buffer_t
{
    void default_values()
    {
        pause_sender = pause_receiver = false;
        sender_dead = receiver_dead = false;
        sender_pos = receiver_pos = 0;
        done = false;
    }

    big_buffer_t()
        : sender_sock(WRONG_FD), receiver_sock(WRONG_FD),
        buf(NULL)
    {
        //default_values();
    }

    big_buffer_t(int sender_sock)
        : sender_sock(sender_sock), receiver_sock(WRONG_FD),
        buf(NULL)
    {
    }

    big_buffer_t(const big_buffer_t& b)
        : sender_sock(b.sender_sock),
        receiver_sock(b.receiver_sock)
    {
        buf = new char[BIG_BUFFER_SIZE];
        default_values();
    }

    ~big_buffer_t()
    {
        delete[] buf;
    }

    int receive()
    {
        int cnt = read(sender_sock, buf + sender_pos,
                BIG_BUFFER_SIZE - sender_pos);
        if (cnt > 0)
        {
            sender_pos += cnt;
        }
        if (cnt < 0)
        {
            perror(BBERROR"receive");
        }
        if (sender_pos == BIG_BUFFER_SIZE)
        {
            pause_sender = true;
        }
        if (sender_pos > receiver_pos)
        {
            pause_receiver = false;
        }
        return cnt;
    }

    int send()
    {
        if (receiver_sock == WRONG_FD)
        {
            std::cerr << "big_buffer_t::send\n" <<
                "no receiver" << std::endl;
            exit(1);
        }
        int cnt = write(receiver_sock, buf + receiver_pos,
                sender_pos - receiver_pos);
        if (cnt > 0)
        {
            receiver_pos += cnt;
        }
        if (cnt < 0)
        {
            perror(BBERROR"send write");
        }
        int n = sender_pos - receiver_pos;
        if (n < 0)
        {
            std::cerr << "This shoudn't have happened" << std::endl;
            exit(2);
        }
        memmove(buf, buf + receiver_pos, n);
        receiver_pos = 0;
        sender_pos = n;
        if (sender_pos == 0)
        {
            pause_receiver = true;
            pause_sender = false;
            if (sender_dead)
            {
                pause_receiver = false;
                done = true;
            }
        }
        return cnt;
    }

    int sender_sock;
    int receiver_sock;
    bool pause_sender, pause_receiver;
    bool sender_dead, receiver_dead;
    bool done;
private:
    static const int WRONG_FD = -1;
    int sender_pos;
    int receiver_pos;
    char *buf;
};

#endif
