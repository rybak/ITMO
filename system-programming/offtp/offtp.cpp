#include <stddef.h>
#include <stdlib.h>
#include <errno.h>

#include <string.h>
#include <sys/types.h>
#include <limits.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include <sys/socket.h>
#include <netdb.h>
#include <sys/epoll.h>
#include <sys/wait.h>

#include <iostream>
//#include <algorithm>

#include "http.h"

#define ERROR "Server Error : "

int epollfd;
int listen_sock;

const size_t MAX_EVENTS = 10;

void init(char *argv[])
{
    listen_sock = init_listen_socket(argv[1], argv[2]);
    epollfd = epoll_create1(0);
    if (epollfd == -1)
    {
        perror("epoll_create");
        exit(EXIT_FAILURE);
    }
    epoll_event ev;
    ev.events = EPOLLIN | EPOLLOUT | EPOLLERR;
    ev.data.fd = listen_sock;
    if (epoll_ctl(epollfd, EPOLL_CTL_ADD, listen_sock, &ev) == -1)
    {
        perror("epoll_ctl: add listen_sock");
        exit(EXIT_FAILURE);
    }
}

void send_error(int conn_sock)
{
    std::cerr << "\t\tsend_error" << std::endl;
    char *msg = strerror(errno);
    write_all(conn_sock, ERR_MSG, MSG_SIZE + 1);
    write_all(conn_sock, msg, strlen(msg) + 1);
    perror(ERROR);
    out_ok(2);
    close(conn_sock);
    exit(EXIT_FAILURE);
}
void send_file_size(int conn_sock, off_t file_size)
{
    std::cerr << "\t\t\tsend filesize == " << file_size << std::endl;
    fprintf(stderr, "0xfs = %lX\n", file_size);
    off_t mask = 0xFF;
    for (int i = 0; i < sizeof(off_t); ++i)
    {
        off_t part = (file_size & mask);
        char c = 0;
        c = (part >> (i * CHAR_BIT));
        write_all(conn_sock, &c, 1);
        mask <<= CHAR_BIT;
    }
}
void send_file(int conn_sock, int fd)
{
    std::cerr << "\t\tsend_file()" << std::endl;
    char *buf = new char[BUF_SIZE];
    off_t sent = 0;
    struct stat fs;
    int fstat_status = fstat(fd, &fs);
    if (fstat_status < 0)
    {
        send_error(conn_sock);
    }
    std::cerr << "\t\t\tsend msg" << std::endl;
    write_all(conn_sock, OK_MSG, MSG_SIZE);
    off_t file_size = fs.st_size;
    send_file_size(conn_sock, file_size);
    std::cerr << "\t\t\tsend file" << std::endl;
    while (sent < file_size)
    {
        int cnt = read(fd, buf, BUF_SIZE);
        if (cnt < 0)
        {
            perror(ERROR"send_file read");
            break;
        }
        if (cnt > 0)
        {
            write_all(conn_sock, buf, cnt);
            sent += cnt;
        }
    }
    if (sent != file_size)
    {
        std::cerr << "Error in middle of file" << std::endl;
        exit(EXIT_FAILURE);
    } else
    {
        std::cerr << "\t\t\tfile sent" << std::endl;
    }
}

void process_client(int conn_sock)
{
    std::cerr << "\tprocess_client" << std::endl;
    char *filename = new char[FN_SIZE];
    std::cerr << "\t\treceiving filename" << std::endl;
    int fn_pos = 0;
    bool eofn = false;
    while (!eofn)
    {
        int cnt = read(conn_sock, filename + fn_pos,
            FN_SIZE - fn_pos);
        if (cnt < 0)
        {
            perror(ERROR"read filename");
            exit(EXIT_FAILURE);
        }
        if (cnt == 0)
        {
            break;
        }
        fn_pos += cnt;
        for (int i = fn_pos - cnt; i < fn_pos; ++i)
        {
            if (filename[i] == 0)
            {
                eofn = true;
                break;
            }
        }
    }
    std::cerr << "\t\topen " << filename << std::endl;
    int fd = open(filename, O_RDONLY);
    if (fd < 0)
    {
        send_error(conn_sock);
    }
    send_file(conn_sock, fd);
    close(conn_sock);
    delete[] filename;
    exit(EXIT_SUCCESS);
}

int main(int argc, char *argv[])
{
    int dpid = fork();
    if (dpid != 0)
    {
        waitpid(dpid, NULL, 0);
        exit(0);
    }
    int sid = setsid();
    if (sid < 0)
    {
        perror(ERROR"creating session ");
        exit(1);
    }
    std::cerr << "server sid == " << sid << std::endl;
    init(argv);
    while (true)
    {
        std::cerr << "accept" << std::endl;
        int conn_sock = accept(listen_sock, NULL, NULL);
        if (conn_sock < 0)
        {
            perror(ERROR"accept");
            continue;
        }
        int spid = fork();
        if (spid != 0)
        {
            std::cerr << "\tspid == " << spid << std::endl;
            close(conn_sock);
            waitpid(spid, NULL, 0);
        } else
        {
            close(listen_sock);
            process_client(conn_sock);
        }
    }
    exit(EXIT_SUCCESS);
}
