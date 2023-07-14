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
#include <poll.h>
#include <sys/select.h>
#include <sys/wait.h>

#include <iostream>

#include <unordered_map>
#include <map>
#include <set>
#include <queue>
#include <string>

#include "http.h"

#define ERROR "Error : sigd : "

#define CMD_LEN 12
const size_t MAX_SOCKETS = 10;
int listen_sock;
pollfd sockets[MAX_SOCKETS];
bool free_position[MAX_SOCKETS];
size_t count_sockets = 0;
struct client_t;

std::unordered_map<int, client_t> & clients();

std::string list("");

void init(char *argv[])
{
    std::cerr << "init" << std::endl;
    listen_sock = init_listen_socket(argv[1], argv[2]);
    sockets[0].fd = listen_sock;
    sockets[0].events = POLLIN | POLLERR;
    sockets[0].revents = 0;
    count_sockets = 1;
    free_position[0] = false;
    for (int i = 1; i < MAX_SOCKETS; ++i)
    {
        free_position[i] = true;
        sockets[i].fd = -1;
    }
}

std::set<std::string> signals;
int max_sign = 0;

void new_signal(const std::string &sig)
{
    std::cerr << "new sig " << sig << std::endl;
    signals.insert(sig);
    list = "";
    for (auto it = signals.begin(); it != signals.end(); ++it)
    {
        list += (*it);
        list += '\n';
    }
}

std::unordered_map<std::string, std::set<int> > subs;
const char LIST[] = "list";
const size_t LIST_LEN = 4;
const char EMIT[] = "emit";
const size_t EMIT_LEN = 4;
const char SUB[] = "sub";
const size_t SUB_LEN = 3;
const char UNSUB[] = "unsub";
const size_t UNSUB_LEN = 5;

struct client_t
{
    client_t(int fd)
        : socket(fd), cmd_pos(0), cmd_ready(false)
    {
        std::cerr << "new client_t" << socket << std::endl;
        cmd = new char[CMD_LEN];
    }
    client_t(const client_t &b)
        : socket(b.socket)
    {
        std::cerr << "client_t copy" << socket << std::endl;
        cmd = new char[CMD_LEN];
    }
    ~client_t()
    {
        std::cerr << "~client_t" << std::endl;
        for (auto it = signals.begin(); it != signals.end(); ++it)
        {
            if (subs.count(*it) > 0)
            {
                subs.at(*it).erase(socket);
                if (subs.at(*it).empty())
                {
                    subs.erase(*it);
                }
            }
        }
        delete[] cmd;
    }
    int socket;
    char *cmd;
    int cmd_pos;
    std::string out;
    int out_pos;
    bool cmd_ready;
    bool sending;
    std::queue<std::string> to_emit;

public:
    int sub()
    {
        const std::string sig(cmd + SUB_LEN);
        if (signals.count(sig) == 0)
        {
            new_signal(sig);
            subs.emplace(sig, std::set<int>());
        }
        subs.at(sig).insert(socket);
        return sig.length();
    }

    int unsub()
    {
        std::string sig(cmd + UNSUB_LEN);
        if (subs.count(sig) > 0)
        {
            subs.at(sig).erase(socket);
            if (subs.at(sig).size() == 0)
            {
                subs.erase(sig);
                signals.erase(sig);
            }
        }
        return sig.length();
    }

    int emit()
    {
        std::string sig(cmd + EMIT_LEN);
        if (signals.count(sig) == 0)
        {
            return sig.length();
        }
        if (subs.count(sig) == 0)
        {
            std::cerr << "This should not have happened" << std::endl;
            exit(2);
        }
        for (auto it = subs.at(sig).begin(); it != subs.at(sig).end(); ++it)
        {
            clients().at(*it).to_emit.push(sig);
        }
        return sig.length();
    }
    void send()
    {
        std::cerr << "out == '" << out << "' out_pos = "
            << out_pos << '\n';
        int cnt = write(socket, out.c_str() + out_pos, out.length() - out_pos + 1);
        if (cnt < 0)
        {
            perror(ERROR"write");
            exit(EXIT_FAILURE);
        }
        if (cnt > 0)
        {
            out_pos += cnt;
        }
        if (cnt == 0)
        {
            std::cerr << "cnt == 0" << std::endl;
            std::cerr << out_pos << std::endl;
        }
        if (out_pos == out.length() + 1)
        {
            sending = false;
            out = "";
            out_pos = 0;
        }
    }
    void handle()
    {
        if (!sending && !to_emit.empty())
        {
            sending = true;
            out = to_emit.back();
            to_emit.pop();
            out_pos = 0;
        }
        if (sending)
        {
            send();
            return;
        }
        if (!cmd_ready)
        {
            read_cmd();
        }
        if (cmd_ready)
        {
            std::cerr << "\t\t\t\tcmd == " << cmd << std::endl;
            int cmd_len = cmd_pos - 1;
            if (strncmp(cmd, LIST, LIST_LEN) == 0)
            {
                std::cerr << "\tcmd == list" << std::endl;
                sending = true;
                out = list;
                out_pos = 0;
                cmd_len = LIST_LEN;
            } else if (strncmp(cmd, SUB, SUB_LEN) == 0)
            {
                std::cerr << "\tcmd == sub" << std::endl;
                int cnt = sub();
                cmd_len = SUB_LEN + cnt;
            } else if (strncmp(cmd, UNSUB, UNSUB_LEN) == 0)
            {
                std::cerr << "\tcmd == unsub" << std::endl;
                int cnt = unsub();
                cmd_len = UNSUB_LEN + cnt;
            } else if (strncmp(cmd, EMIT, EMIT_LEN) == 0)
            {
                std::cerr << "\tcmd == emit" << std::endl;
                int cnt = emit();
                cmd_len = EMIT_LEN + cnt;
            }
            memmove(cmd, cmd + cmd_len + 1, cmd_pos - cmd_len - 1);
            cmd_pos -= cmd_len + 1;
            cmd_ready = false;
        }
    }
private:
    void read_cmd()
    {
        std::cerr << "read_cmd cnt == ";
        int cnt = read(socket, cmd + cmd_pos, CMD_LEN - cmd_pos);
        std::cerr << cnt << std::endl;
        if (cnt < 0)
        {
            perror(ERROR"read");
            exit(EXIT_FAILURE);
        }
        if (cnt > 0)
        {
            cmd_pos += cnt;
            for (int i = cmd_pos - cnt; i < cmd_pos; ++i)
            {
                if (cmd[i] == 0)
                {
                    cmd_ready = true;
                    break;
                }
            }
        }
        if (cnt == 0)
        {
            cmd_ready = true;
        }
    }
};
std::unordered_map<int, client_t> & clients()
{
    static std::unordered_map<int, client_t> res;
    return res;
}
void add_connection()
{
    int conn_sock = accept(listen_sock, NULL, NULL);
    std::cerr << "accept " << conn_sock << std::endl;
    if (conn_sock < 0)
    {
        perror(ERROR"accept");
        return;
    }
    clients().emplace(conn_sock, client_t(conn_sock));
    int curr = 1;
    while (curr < MAX_SOCKETS && !free_position[curr])
    {
        ++curr;
    }
    if (curr < MAX_SOCKETS)
    {
        sockets[curr].fd = conn_sock;
        sockets[curr].events = POLLERR | POLLIN | POLLOUT;
        sockets[curr].revents = 0;
        free_position[curr] = false;
        ++count_sockets;
    }
}

void close_connection(int fd)
{
    std::cerr << "close " << fd << std::endl;
    close(fd);
    clients().erase(fd);
}

void handle_client(int socket)
{
    std::cerr << "handle " << socket << std::endl;
    clients().at(socket).handle();
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
    std::cerr << "sigd server sid == " << sid << std::endl;
    init(argv);
    while (true)
    {
        std::cerr << "poll";
        int poll_count = poll(sockets, MAX_SOCKETS, -1);
        for (int i = 0; i < MAX_SOCKETS; ++i)
        {
            std::cerr << i << ' ' << sockets[i].revents << std::endl;
        }
        if (poll_count == 0)
        {
            continue;
        }
        if (poll_count < 0)
        {
            if (poll_count == EINTR)
            {
                perror("poll interupted");
                continue;
            }
            perror(ERROR"poll");
            exit(EXIT_FAILURE);
        }
        if (sockets[0].revents & POLLIN)
        {
            if (count_sockets < MAX_SOCKETS)
            {
                add_connection();
                std::cerr << "continue " << std::endl;
                continue;
            }
        }
        std::cerr << "poll_count == " << poll_count << std::endl;
        for (int i = 1; i < MAX_SOCKETS; ++i)
        {
            if (sockets[i].fd > 0)
            {
                std::cerr << "polled " << sockets[i].fd << std::endl;
                std::cerr << "events " << sockets[i].events << std::endl;
                std::cerr << "revents " << sockets[i].revents << std::endl;
                std::cerr << "\tPOLLIN " << POLLIN << std::endl;
                std::cerr << "\tPOLLERR " << POLLERR << std::endl;
                std::cerr << "\tPOLLOUT " << POLLOUT << std::endl;
                std::cerr << "\tPOLLHUP " << POLLHUP << std::endl;
                std::cerr << "\tPOLLNVAL" << POLLNVAL << std::endl;
std::cerr << " & == " <<
    (sockets[i].revents & (POLLERR) ) << std::endl;
                if (sockets[i].revents & (POLLERR) )
                {
                    std::cerr << "Error while listening socket" << std::endl;
                    close_connection(sockets[i].fd);
                    sockets[i].fd = -1;
                    sockets[i].events = 0;
                    free_position[i] = true;
                    continue;
                }
                if (sockets[i].revents & (POLLIN | POLLOUT))
                {
                    int sockfd = sockets[i].fd;
                    handle_client(sockfd);
                }
            }
        }
    }
    exit(EXIT_SUCCESS);
}

