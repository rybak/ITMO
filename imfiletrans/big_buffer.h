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
        std::cerr << "\t\tbig_buffer_t()" << std::endl;
        default_values();
    }

    big_buffer_t(int sender_sock)
        : sender_sock(sender_sock), receiver_sock(WRONG_FD),
        buf(NULL)
    {
        std::cerr << "\t\tbig_buffer_t(int)" << std::endl;
        default_values();
    }

    big_buffer_t(const big_buffer_t& b)
        : sender_sock(b.sender_sock),
        receiver_sock(b.receiver_sock)
    {
        std::cerr << "\t\tbig_buffer_t(big_buffer_t)" << std::endl;
        buf = new char[BIG_BUFFER_SIZE];
        default_values();
    }

    ~big_buffer_t()
    {
        std::cerr << "\t\tbig_buffer_t::~~~~" << std::endl;
        delete[] buf;
    }

    int receive()
    {
        std::cerr << "\t\tbig_buffer_t::receive\n";
        std::cerr << "\t\t\tpos = " << sender_pos << std::endl;
        int cnt = read(sender_sock, buf + sender_pos,
                BIG_BUFFER_SIZE - sender_pos);
        std::cerr << "\t\t\tfile : ";
        write(2, buf, 20);
        std::cerr << std::endl;
        if (cnt > 0)
        {
            sender_pos += cnt;
        }
        if (cnt < 0)
        {
            perror("\t\tERROR : big_buffer_t::receive read");

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
        std::cout << "big_buffer_t::send" << std::endl;
        int cnt = write(receiver_sock, buf + receiver_pos,
                sender_pos - receiver_pos);
        if (cnt > 0)
        {
            receiver_pos += cnt;
        }
        if (cnt < 0)
        {
            perror("big_buffer_t::send write");
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
