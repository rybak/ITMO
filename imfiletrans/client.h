#ifndef CLIENT_H
#define CLIENT_H

struct client
{
    explicit client() : state(UNDEF)
    {}

    void process_client();

private:
    static const int IN = 0, OUT = 1, UNDEF = -1;
    static const size_t FILE_SIZE = 32 * 1024 * 1024; // 32M
    int state;
    char *buf;
    int input, output;

    void process_undef();
    void process_in();
    void process_out();
};


#endif
