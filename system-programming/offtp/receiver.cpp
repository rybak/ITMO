#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>

#include <iostream>

#include <sys/socket.h>
#include <netdb.h>
#include <sys/epoll.h>
#include <sys/wait.h>

#include "http.h"

#define ERROR "Receiver error : " 
int sockfd;

void receive_error_message()
{
    char *err_msg = new char[BUF_SIZE];
    std::cerr << "\t\treceiving error message" << std::endl;
    int pos = 0;
    bool eoem = false;
    while (!eoem)
    {
        int cnt = read(sockfd, err_msg + pos, BUF_SIZE - pos);
        if (cnt < 0)
        {
            perror(ERROR"read error_msg");
            exit(EXIT_FAILURE);
        }
        if (cnt == 0)
        {
            break;
        }
        pos += cnt;
        for (int i = pos - cnt; i < pos; ++i)
        {
            if (err_msg[i] == 0)
            {
                eoem = true;
                break;
            }
        }
    }
    std::cerr << "\t\t"ERROR << err_msg << std::endl;
    delete[] err_msg;
    exit(EXIT_FAILURE);
}

void receive_file_size(int sockfd, off_t &file_size)
{
    std::cerr << "\treceiving filesize" << std::endl;
//    read_all(sockfd, (char *)&file_size, sizeof(off_t));
    
    int pos = 0;
    file_size = 0;
    off_t mask = 0xFF;
    for (int i = 0; i < sizeof(off_t); ++i)
    {
        char c;
        read_all(sockfd, &c, 1);
        unsigned long int part = (unsigned char) c;
        part <<= pos;
        file_size |= part;
        pos += CHAR_BIT;
    }
}

int main(int argc, char *argv[])
{
    std::cerr << "receiver" << std::endl;
    sockfd = init_connect_socket(argv[1], argv[2]);
    std::cerr << "\tsending filename" << std::endl;
    char *filename = argv[3];
    std::cerr << "\t\tfilename == " << filename << std::endl;
    write_all(sockfd, filename, strlen(filename) + 1);
    std::cerr << "\treceiving message" << std::endl;
    char msg[MSG_SIZE + 1];
    read_all(sockfd, msg, MSG_SIZE);
    msg[MSG_SIZE] = 0;
    std::cerr << "\t\tmessage : " << msg << std::endl;
    if (strncmp(ERR_MSG, msg, MSG_SIZE) == 0)
    {
        receive_error_message();
    }
    if (strncmp(OK_MSG, msg, MSG_SIZE) != 0)
    {
        std::cerr << "\t\twrong message" << std::endl;
        exit(EXIT_FAILURE);
    }
    off_t file_size;
    receive_file_size(sockfd, file_size);
    std::cerr << "\t\tfilesize == " << file_size << std::endl;
    std::cerr << "\treceiving file" << std::endl;
    char *buf = new char[BUF_SIZE];
    off_t received = 0;
    while (received < file_size)
    {
        int cnt = read(sockfd, buf, BUF_SIZE);
        if (cnt == 0)
        {
            std::cerr << "\t\tend of stream" << std::endl;
            break;
        }
        if (cnt < 0)
        {
            perror("read");
            exit(EXIT_FAILURE);
        }
        received += cnt;
        write_all(1, buf, cnt);
    }
    delete[] buf;
    if (received != file_size)
    {
        std::cerr << "Error in middle of file" << std::endl;
        exit(EXIT_FAILURE);
    } else
    {
        std::cerr << "\tfile received" << std::endl;
        exit(EXIT_SUCCESS);
    }
}

