#include <cstring>
#include "client.h"
#include "big_buffer.h"

namespace
{
    std::unordered_map<token_t, big_buffer_t>& buffers()
    {
        static std::unordered_map<token_t, big_buffer_t> res;
        return res;
    }
}


client::client(int events, int fd)
    : state(RECEIVING_MSG), fd(fd), buf(fd, msg, MSG_SIZE)
{
    if (events & EPOLLIN)
    {
        type = SENDER;
        return;
    }
    if (events & EPOLLOUT)
    {
        type = RECEIVER;
        return;
    }
}

void client::process_client()
{
    switch(state)
    {
    case RECEIVING_MSG:
        if (buf.read() == 0)
        {
            check_msg();
            switch (type)
            {
            case SENDER:
                token = create_token();
                state = SENDING_TOKEN;
                break;
            case RECEIVER:
                state = GETTING_TOKEN;
                break;
            }
            buf = async_buf(fd, &token, TOKEN_SIZE);
        }
        break;
    case RECEIVING_TOKEN:
        if (!buf.read())
        {
            process_token();
            state = SENDING_FILE;
        }
        break;
    case SENDING_TOKEN:
        if (!buf.write())
        {
            state = RECEIVING_FILE;
        }
        break;
    }
}

void client::check_msg()
{
    bool ok = true;
    switch(type)
    {
    case SENDER:
        ok = strncmp(msg, SEND_MSG, MSG_SIZE) == 0;
        break;
    case RECEIVER:
        ok = strncmp(msg, RECV_MSG, MSG_SIZE) == 0;
        break;
    }
    if (!ok)
    {
        std::cerr << "Wrong message : " << msg << std::endl;
        exit(1);
    }
}

