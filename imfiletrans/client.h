#ifndef CLIENT_H
#define CLIENT_H

struct async_buf
{
    async_buf(int fd, char *buf, int cnt)
        : fd(fd), buf(buf), n(cnt)
    {}

    int read()
    {
        return rw(read);
    }

    int write()
    {
        return rw(write);
    }
private:
    int fd;
    int pos, n;
    char *buf;
    int rw(int (*f)(int,char*,int))
    {
        if (n == 0)
        {
            return 0;
        }
        int cnt = f(fd, buf + pos, n);
        n -= cnt;
        pos += cnt;
        if (cnt < 0)
        {
            perror("async_buf");
            exit(1);
        }
        return cnt;
    }
};

typedef token_t int;

struct client
{
    explicit client(int, int);
    void process_client();

private:
    enum state_t
    {
        RECEIVING_MSG, RECEIVING_TOKEN, SENDING_TOKEN,
        RECEIVING_FILE, SENDING_FILE
    } state;
    enum client_type_t { SENDER, RECEIVER };
    token_t token;
    int input, output;
    int fd;
    async_buf buf;
    char msg[MSG_SIZE];
};


#endif
