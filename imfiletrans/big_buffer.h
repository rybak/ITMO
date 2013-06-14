#ifndef BIG_BUFFER_H
#define BIG_BUFFER_H

const size_t BIG_BUFFER_SIZE = 32 * 1024 * 1024;

struct big_buffer_t
{
    big_buffer_t(int sender_sock)
        : sender_pos(0), reciever_pos(0), sender_sock(sender_sock),
          reciever_sock(WRONG_FD);
    { }

    int recieve()
    {
        int cnt = read(sender_sock, buf + sender_pos,
                BIG_BUFFER_SIZE - sender_pos);
        sender_pos += cnt;
        return cnt;
    }

    int send()
    {
        if (reciever_sock == WRONG_FD)
        {
            std::cerr << "big_buf_t::send\n" <<
                "no recevier" << std::endl;
            exit(1);
        }
        int cnt = write(reciever_sock, buf + reciever_pos,
                sender_pos - reciever_pos);
        reciever_pos += cnt;
        return cnt;
    }
private:
    const int WRONG_FD = -1;
    int sender_pos;
    int sender_sock;
    int reciever_pos;
    int reciever_sock;
    chat *buf[BIG_BUFFER_SIZE];
};

#endif
