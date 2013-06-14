#include "client.h"

client::client(int events, int fd)
    : state(GET_MSG), fd(fd)
{
    if (events & EPOLLIN)
    {
        type = IN;
        return;
    }
    if (events & EPOLLOUT)
    {
        type = OUT;
        return;
    }
}

void client::process_client()
{
    switch(state)
    {
    case GET_MSG:
        buf = async_buf(fd, msg, MSG_SIZE);
        state = GETTING_MSG;
        break;
    case GETTING_MSG:
        if (!buf.read())
        {
            check_msg();
            switch (type)
            {
            case IN:
                state = SEND_TOKEN;
                break;
            case OUT:
                state = GET_TOKEN;
                break;
            }
        }
        break;
    case GET_TOKEN:
        buf = async_buf(fd, &token, TOKEN_SIZE);
        state = GETTING_TOKEN;
        break;
    case GETTING_TOKEN:
        if (!buf.read())
        {
            process_token();
            state = WRITING;
        }
        break;
    case READING:
        if (!buf.read())
        {

        }
        break;
    case OUT:
        process_out();
        break;
    default:
        std::cerr << "THIS SHOULD NOT HAVE HAPPENED" << std::endl;
        exit(1);
    }
}

void client::process_in()
