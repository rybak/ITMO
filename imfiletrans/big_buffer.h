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
        : sender_sock(WRONG_FD), receiver_sock(WRONG_FD)
    {}
    big_buffer_t(int sender_sock)
        : sender_pos(0), receiver_pos(0), sender_sock(sender_sock),
          receiver_sock(WRONG_FD)
    { }

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
        return cnt;
    }

    const bool need_pause_sender() const
    {
        return pause_sender;
    }
    const bool need_pause_receiver() const
    {
        return pause_receiver;
    }

private:
    static const int WRONG_FD = -1;
    int sender_pos;
    int sender_sock;
    int receiver_pos;
    int receiver_sock;
    bool pause_sender, pause_receiver;
    char *buf[BIG_BUFFER_SIZE];
};

#endif
