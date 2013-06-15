#ifndef BIG_BUFFER_H
#define BIG_BUFFER_H
#include <stddef.h>
#include <stdlib.h>
#include <cstring>
#include <unistd.h>
#include <fcntl.h>
#include <iostream>

const size_t BIG_BUFFER_SIZE = 32 * 1024 * 1024;

struct big_buffer_t
{
    big_buffer_t()
        : sender_sock(WRONG_FD), receiver_sock(WRONG_FD),
        pause_receiver(false), pause_sender(false)
    {
        buf = new char[BIG_BUFFER_SIZE];
    }

    big_buffer_t(int sender_sock)
        : sender_pos(0), receiver_pos(0),
        sender_sock(sender_sock), receiver_sock(WRONG_FD),
        pause_receiver(false), pause_sender(false)
    {
        buf = new char[BIG_BUFFER_SIZE];
    }

    ~big_buffer_t()
    {
        delete[] buf;
    }

    void set_receiver(int sock)
    {
        receiver_sock = sock;
    }
    int receive()
    {
        int cnt = read(sender_sock, buf + sender_pos,
                BIG_BUFFER_SIZE - sender_pos);
        if (cnt > 0)
        {
            sender_pos += cnt;
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
            std::cerr << "big_buf_t::send\n" <<
                "no receiver" << std::endl;
            exit(1);
        }
        std::cout << "big_buf_t::send" << std::endl;
        int cnt = write(receiver_sock, buf + receiver_pos,
                sender_pos - receiver_pos);
        if (cnt > 0)
        {
            receiver_pos += cnt;
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
        }
        return cnt;
    }

    int sender_sock;
    int receiver_sock;
    bool pause_sender, pause_receiver;
private:
    static const int WRONG_FD = -1;
    int sender_pos;
    int receiver_pos;
    char *buf;
};

#endif
