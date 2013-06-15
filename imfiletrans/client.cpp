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

void client::default_values()
{
    msg = new char[MSG_SIZE];
    msg_pos = token_pos = 0;
    pause = false;
    wake_up = WRONG_FD;
}

client::client(int events, int fd)
    : state(RECEIVING_MSG), fd(fd)
{
    if (!((events & EPOLLIN) || (events & EPOLLOUT)))
    {
        std::cerr << "Client constructor" << std::endl;
        exit(2);
    }
    default_values();
    strncpy(msg, "DEAD", MSG_SIZE);
}

client::client() : state(UNDEFINED), fd(WRONG_FD), msg(NULL)
{ }

client::client(const client& b)
    : state(b.state), fd(b.fd), token(b.token)
{
    default_values();
    strncpy(msg, "DEAD", MSG_SIZE);
}

client::~client()
{
    delete[] msg;
}

void client::read_msg()
{
    int cnt = ::read(fd, msg + msg_pos, MSG_SIZE - msg_pos);
    if (cnt < 0)
    {
        perror("read_msg");
        exit(1);
    }
    msg_pos += cnt;
}

void client::read_token()
{
    int cnt =
        ::read(fd, (&token) + token_pos, TOKEN_SIZE - token_pos);
    if (cnt < 0)
    {
        perror("read_token");
        exit(1);
    }
    token_pos += cnt;
}

void client::write_token()
{
    int cnt =
        ::write(fd, (&token) + token_pos, TOKEN_SIZE - token_pos);
    if (cnt < 0)
    {
        perror("write_token");
        exit(1);
    }
    token_pos += cnt;
}

void client::process_client()
{
    std::cout << "process_client " << fd << std::endl;
    std::cout.flush();
    switch(state)
    {
    case RECEIVING_MSG:
        std::cout << "RECEIVING_MSG" << std::endl;
        read_msg();
        if (msg_pos == MSG_SIZE)
        {
            check_msg();
            std::cout << "good message \"" << msg << '"' << std::endl;
            switch (type)
            {
            case SENDER:
                std::cout << "creating token... ";
                std::cout.flush();
                token = create_token();
                std::cout << token << std::endl;
                state = SENDING_TOKEN;
                break;
            case RECEIVER:
                state = RECEIVING_TOKEN;
                break;
            }
        }
        break;
    case RECEIVING_TOKEN:
        std::cout << "RECEIVING_TOKEN" << std::endl;
        read_token();
        if (token_pos == TOKEN_SIZE)
        {
            process_token();
            state = SENDING_FILE;
        }
        break;
    case SENDING_TOKEN:
        std::cout << "SENDING_TOKEN" << std::endl;
        write_token();
        if (token_pos == TOKEN_SIZE)
        {
            std::cout << "token sent " << token << std::endl;
            buffers().emplace(token, big_buffer_t(fd));
            state = RECEIVING_FILE;
        }
        break;
    case RECEIVING_FILE:
    {
        std::cout << "RECEIVING_FILE" << std::endl;
        int cnt = buffers()[token].receive();
        if (buffers().at(token).pause_sender)
        {
            pause = true;
        }
        if (!buffers().at(token).pause_receiver)
        {
            wake_up = buffers().at(token).receiver_sock;
        } else
        {
            wake_up = WRONG_FD;
        }
        if (cnt <= 0)
        {
            state = DEAD;
        }
    }
        break;
    case SENDING_FILE:
    {
        int cnt = buffers().at(token).send();
        if (!buffers().at(token).pause_sender)
        {
            wake_up = buffers().at(token).sender_sock;
        } else
        {
            wake_up = WRONG_FD;
        }
        if (buffers().at(token).pause_receiver)
        {
            pause = true;
        }
        if (cnt <= 0)
        {
            state = DEAD;
        }
    }
        break;
    case UNDEFINED:
        std::cerr << "UNDEFINED state" << std::endl;
        exit(2);
        break;
    }
    out_ok();
}

void client::check_msg()
{
    bool ok = true;
    if (ok = strncmp(msg, SEND_MSG, MSG_SIZE) == 0)
    {
        type = SENDER;
    } else
    {
        if (ok = strncmp(msg, RECV_MSG, MSG_SIZE) == 0)
        {
            type = RECEIVER;
        }
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
    } else
    {
        state = DEAD;
    }
}
