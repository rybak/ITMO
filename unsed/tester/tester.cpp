#include <cstdlib>
#include <ctime>
#include <string>
#include <iostream>
#include <fstream>
#include <fcntl.h>
#include <wait.h>
#include <unistd.h>
#include <signal.h>

using std::string;
using std::cout;
using std::cerr;
using std::endl;

const size_t pause_size = 5;

const string rand_str(size_t len) {
    string res(len, ' ');
    for (size_t i = 0; i < len; ++i) {
        res[i] = 'a' + rand() % ('z' - 'a' + 1);
    }
    return res;
}

void make_pause() {
    sleep(pause_size);
}

void test(size_t n) {
    const int pause_prob = 5;
    const size_t test_size = 10;
    for (size_t t = 0; t < test_size; ++t) {
        size_t len = rand() % (2 * n);
        string s = rand_str(len);
        if (len > 1) {
            size_t pause_pos = rand() % (len - 1) + 1;
            s[pause_pos - 1] = 'P';
            cout << s.substr(0, pause_pos);
            if (rand() % pause_prob == 0) {
                make_pause();
            }
            cout << s.substr(pause_pos, len - pause_pos);
        }
        cout << endl;
    }
    close(1);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        cout << "Usage: tester K\n";
        return 1;
    }
    const size_t K = atoi(argv[1]);
    const size_t n = rand() % (4 * K);
    int fds[2];
    int pid;
    const string cmd_prefix("../");
    const string cmd_run("readlines-main");
    const string cmd = cmd_prefix + cmd_run;
    const string nstr = std::to_string(n);
    size_t i = 0;
    while (1) {
        pipe(fds);
        if (pid = fork()) {
            int test_pid;
            if (test_pid = fork()) {
                close(fds[0]);
                close(fds[1]);
            } else {
                dup2(fds[1], 1);
                close(fds[0]);
                close(fds[1]);
                srand(i);
                test(n);
                return 0;
            }
            int status;
            cout << "Test #" << i << ' ';
            ++i;
            cout << "Waiting for readlines ";
            cout.flush();
            int timer_pid = fork();
            if (!timer_pid) {
                int t = 0;
                while (t < pause_size) {
                    cout << '.';
                    cout.flush();
                    sleep(1);
                }
                cerr << "\n\nERROR: " << cmd_run
                    << "' is running to long.\n\n" << endl;
                return 1;
            }
            waitpid(pid, &status, 0);
            kill(timer_pid, SIGTERM);
            cout << "waiting finished" << endl;
            if (!WIFEXITED(status)) {
                cerr << "Something wrong";
                return 1;
            }
            if (WEXITSTATUS(status) != 0) {
                cerr << "Something wrong 2";
                return 1;
            }
        } else {
            dup2(fds[0], 0);
            close(fds[0]);
            close(fds[1]);
            execl(cmd.c_str(), cmd_run.c_str(), nstr.c_str(), NULL);
            perror("Error while 'execl' occurred: ");
            return 1;
        }
    }
    return 0;
}

