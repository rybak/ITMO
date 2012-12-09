#include <cstdlib>
#include <ctime>
#include <string>
#include <iostream>
#include <fstream>
#include <fcntl.h>
#include <wait.h>
#include <unistd.h>
using std::string;
using std::cout;
using std::cerr;
using std::endl;

const string rand_str(size_t len) {
    string res(len, ' ');
    for (size_t i = 0; i < len; ++i) {
        res[i] = 'a' + rand() % ('z' - 'a' + 1);
    }
    return res;
}

void make_pause() {
    sleep(1);
}

void test(size_t n) {
    const size_t test_size = 10;
    for (size_t t = 0; t < test_size; ++t) {
        size_t len = rand() % (2 * n);
        string s = rand_str(len);
        if (len > 0) {
            size_t pause_pos = rand() % len + 1;
            s[pause_pos - 1] = 'P';
            cout << s.substr(0, pause_pos);
            if (rand() % 5 == 0) {
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
        std::cout << "Usage: tester K\n";
        return 1;
    }
    srand(time(NULL));
    size_t K = atoi(argv[1]);
    size_t n = rand() % (4 * K);
    {
        std::ofstream stream("n.dat");
        stream << n;
        stream.close();
    }
    int fds[2];
    int pid;
    string cmd("../readlines-main");
    string nstr = std::to_string(n);
    size_t i = 0;
    while (1) {
        pipe(fds);
        if (pid = fork()) {
            int tpid;
            if (tpid = fork()) {
                dup2(fds[1], 1);
                close(fds[0]);
                close(fds[1]);
                int status;
                cerr << "test..." << endl;
                waitpid(tpid, &status, 0);
                cerr << "test OK" << endl;
            } else {
                test(n);
                return 0;
            }
            int status;
            cerr << "Waiting..." << endl;
            waitpid(pid, &status, 0);
            cerr << "readlines : OK" << endl;
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
            cout << "Test #" << i << ' ';
            ++i;
            // cout << cmd << ' ' << nstr << endl;
            execl(cmd.c_str(), cmd.c_str(), nstr.c_str(), NULL);
        }
    }
    return 0;
}

