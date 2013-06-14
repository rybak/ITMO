#include <cstring>
#include <unordered_map>
#include <iostream>
#include "client.h"
#include "big_buffer.h"
#include "http.h"
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
                state = RECEIVING_TOKEN;
                break;
            }
            buf = async_buf(fd, (char *)&token, TOKEN_SIZE);
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
            buffers()[token] = big_buffer_t(fd);
            state = RECEIVING_FILE;
        }
        break;
    case RECEIVING_FILE:
    {
        int cnt = buffers()[token].receive();
        if (buffers()[token].need_pause_sender())
        {
            // TODO receiver not catching up
            // buffer stuffed up
        }
        if (!buffers()[token].need_pause_receiver())
        {
            // TODO wake up receiver
        }
        if (cnt <= 0)
        {
            // TODO end of received file
        }
    }
        break;
    case SENDING_FILE:
    {
        int cnt = buffers()[token].send();
        if (!buffers()[token].need_pause_sender())
        {
            // TODO wake up sender
        }
        if (buffers()[token].need_pause_receiver())
        {
            // TODO receiver not catching up
        }
        if (cnt <= 0)
        {
            // TODO end of sended file
        }
    }
        break;
    case UNDEFINED:
        std::cerr << "UNDEFINED state" << std::endl;
        exit(2);
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

int client::create_token()
{
    return fd;
}

void client::process_token()
{
    if (buffers().count(token) > 0)
    {
        buffers()[token].set_receiver(fd);
    }
}
