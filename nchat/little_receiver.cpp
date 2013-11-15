#include "little_receiver.h"
#include <stdexcept>
#include <sys/socket.h>
#include <unistd.h>
#include <cstring>

little_receiver::little_receiver(int sock)
    : buffer(NULL)
{
    conn_sock = accept(sock, NULL, NULL);
    if (conn_sock < 0)
    {
        throw std::runtime_error("little_receiver::little_receiver : accept");
    }
}

little_receiver::~little_receiver()
{
    delete[] buffer;
    close(conn_sock);
}

chat_message little_receiver::receive_message()
{
    int cnt = 0;
    cm_header h;
    cnt = recvfrom(conn_sock, &h, sizeof(h), 0, NULL, NULL);
    if (cnt < 0)
    {
        throw std::runtime_error("little_receiver::receive_message : recvfrom header");
    }
    chat_message msg(h);
    msg.to_host();
    if (msg.len > 10000 || msg.len < 0)
    {
        msg.text = "bad len";
        return msg;
    }
    uint32_t len = msg.len;
    buffer = new char[len + 1];
    memset(buffer, 0, len + 1);
    cnt = recvfrom(conn_sock, buffer, len, 0, NULL, NULL);
    if (cnt < 0)
    {
        throw std::runtime_error("little_receiver::receive_message : recvfrom message");
    }
    buffer[len] = 0;
    msg.text = buffer;
    return msg;
}
