#ifndef CLIENT_H
#define CLIENT_H

namespace
{
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
}

struct client
{
    explicit client(int, int);
    void process_client();

private:
    static const int IN = 0, OUT = 1;
    static const size_t FILE_SIZE = 32 * 1024 * 1024; // 32M
    enum state_t
    {
        GET_MSG, GETTING_MSG,
        GET_TOKEN, GETTING_TOKEN,
        SEND_TOKEN, SENDING_TOKEN,
        READING, WRITING
    } state;
    int type;
    int token;
    int input, output;
    async_buf buf;
    char msg[MSG_SIZE];
    void process_undef();
    void process_in();
    void process_out();
};


#endif
