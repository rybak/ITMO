#include <cstring>
#include <unordered_map>
#include <iostream>
#include "client.h"
#include "big_buffer.h"
#include "http.h"

std::unordered_map<token_t, big_buffer_t>& buffers()
{
    static std::unordered_map<token_t, big_buffer_t> res;
    return res;
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
void client::process_client()
{
    std::cerr << "\tprocess_client " << fd << std::endl;
    switch(state)
    {
    case RECEIVING_MSG:
        receive_msg();
        break;
    case RECEIVING_TOKEN:
        receive_token();
        break;
    case SENDING_TOKEN:
        send_token();
        break;
    case RECEIVING_FILE:
        receive_file();
        break;
    case SENDING_FILE:
        send_file();
        break;
    case UNDEFINED:
        std::cerr << ERROR"UNDEFINED state" << std::endl;
        exit(2);
        break;
    case DEAD:
        std::cerr << "\tWAT?! 0_0" << std::endl;
        exit(2);
        break;
    }
}

void client::receive_msg()
{
    std::cerr << "\t\tRECEIVING_MSG" << std::endl;
    read_msg();
    if (msg_pos == MSG_SIZE)
    {
        if (check_msg())
        {
            process_msg();
        } else
        {
            std::cerr << "\t\t\tWrong message : " << msg << std::endl;
            state = DEAD;
        }
    }
}

void client::read_msg()
{
    std::cerr << "\t\t\tread_msg" << std::endl;
    int cnt = ::read(fd, msg + msg_pos, MSG_SIZE - msg_pos);
    if (cnt < 0)
    {
        perror(ERROR"read_msg");
        exit(1);
    }
    msg_pos += cnt;
}

bool client::check_msg()
{
    if (strncmp(msg, SEND_MSG, MSG_SIZE - 1) == 0)
    {
        type = SENDER;
        return true;
    }
    if (strncmp(msg, RECV_MSG, MSG_SIZE - 1) == 0)
    {
        type = RECEIVER;
        return true;
    }
    return false;
}

void client::process_msg()
{
    std::cerr << "\t\t\tgood message \"" << msg << '"' << std::endl;
    switch (type)
    {
    case SENDER:
        token = create_token();
        std::cerr << token << std::endl;
        state = SENDING_TOKEN;
        break;
    case RECEIVER:
        state = RECEIVING_TOKEN;
        break;
    }
}

int client::create_token()
{
    std::cerr << "\t\t\tcreating token... ";
    return fd;
}

void client::receive_token()
{
    std::cerr << "\tRECEIVING_TOKEN" << std::endl;
    read_token();
    if (token_pos == TOKEN_SIZE)
    {
        if (check_token())
        {
            std::cerr << "\t\tGood token : " << token << std::endl;
            state = SENDING_FILE;
        } else
        {
            std::cerr << "\t\tBad token." << std::endl;
            state = DEAD;
        }
    }
}

void client::read_token()
{
    int cnt =
        ::read(fd, (&token) + token_pos, TOKEN_SIZE - token_pos);
    if (cnt < 0)
    {
        perror(ERROR"read_token");
        exit(1);
    }
    token_pos += cnt;
}

bool client::check_token()
{
    if (buffers().count(token) > 0)
    {
        if (buffers().at(token).receiver_sock == WRONG_FD)
        {
            buffers().at(token).receiver_sock = fd;
            return true;
        } else
        {
            std::cerr << "\t\tToken " << token
                << " is taken." << std::endl;
            return false;
        }
    } else
    {
        std::cerr << "There is no token == " << token << std::endl;
        return false;
    }
}

void client::send_token()
{
    std::cerr << "\tSENDING_TOKEN" << std::endl;
    write_token();
    if (token_pos == TOKEN_SIZE)
    {
        std::cerr << "\t\ttoken sent " << token << std::endl;
        buffers().emplace(token, big_buffer_t(fd));
        state = RECEIVING_FILE;
    }
}

void client::write_token()
{
    int cnt =
        ::write(fd, (&token) + token_pos, TOKEN_SIZE - token_pos);
    if (cnt < 0)
    {
        perror(ERROR"write_token");
        exit(1);
    }
    token_pos += cnt;
}

void client::receive_file()
{
    std::cerr << "\tRECEIVING_FILE" << std::endl;
    int cnt = buffers().at(token).receive();
    pause = false;
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
    if ((cnt < 0) || (cnt == 0 && !pause))
    {
        state = DEAD;
        wake_up = buffers().at(token).receiver_sock;
    }
}

void client::send_file()
{
    std::cerr << "\tSENDING_FILE" << std::endl;
    int cnt = buffers().at(token).send();
    pause = false;
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
    if (buffers().at(token).done)
    {
        std::cerr << "\t\tDONE" << std::endl;
        state = DEAD;
    }
    if ((cnt < 0) || (cnt == 0 && !pause))
    {
        state = DEAD;
    }
}

