#ifndef BIG_BUFFER_H
#define BIG_BUFFER_H

const size_t BIG_BUFFER_SIZE = 32 * 1024 * 1024;

struct big_buffer_t
{
    big_buffer_t(int sender_sock)
        : sender_pos(0), receiver_pos(0), sender_sock(sender_sock),
          receiver_sock(WRONG_FD);
    { }

    int receive()
    {
        int cnt = read(sender_sock, buf + sender_pos,
                BIG_BUFFER_SIZE - sender_pos);
        sender_pos += cnt;
        if (sender_pos == BIG_BUFFER_SIZE)
        {
            pause_sender = true;
        }
        return cnt;
    }

    int send()
    {
        if (receiver_sock == WRONG_FD)
        {
            std::cerr << "big_buf_t::send\n" <<
                "no receveir" << std::endl;
            exit(1);
        }
        int cnt = write(receiver_sock, buf + receiver_pos,
                sender_pos - receiver_pos);
        receiver_pos += cnt;
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
    const int WRONG_FD = -1;
    int sender_pos;
    int sender_sock;
    int receiver_pos;
    int receiver_sock;
    bool pause_sender, pause_receiver;
    chat *buf[BIG_BUFFER_SIZE];
};

#endif
