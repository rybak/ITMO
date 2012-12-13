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
using std::cin;

const size_t pause_time = 5;

const string rand_str(size_t len) {
    string res(len, ' ');
    for (size_t i = 0; i < len; ++i) {
        res[i] = 'a' + rand() % ('z' - 'a' + 1);
    }
    return res;
}

void make_pause() {
    sleep(pause_time);
}

void print_test(size_t n) {
    const int pause_prob = 5;
    int pause_been = 0;
    const size_t test_size = 10;
    for (size_t t = 0; t < test_size; ++t) {
        size_t len = rand() % (2 * n);
        string s = rand_str(len);
        if (len > 1) {
            size_t pause_pos = rand() % (len - 1) + 1;
            s[pause_pos - 1] = 'P';
            cout << s.substr(0, pause_pos);
            if ((rand() % pause_prob == 0) && !pause_been) {
                pause_been = 1;
                make_pause();
            }
            cout << s.substr(pause_pos, len - pause_pos);
        }
        cout << endl;
    }
    close(1);
}

void close_pipe(int fd[]) {
    close(fd[0]);
    close(fd[1]);
}

void alarm_handler(int signum) {
}

void init_signals() {
    struct sigaction new_action;
    if (sigemptyset(&new_action.sa_mask)) {
        perror("Can't use sigemptyset.");
        exit(-1);
    }
    new_action.sa_handler = alarm_handler;
    if (sigaction(SIGALRM, &new_action, NULL)) {
        perror("Can't use sigaction");
        exit(-1);
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        cout << "Usage: tester K\n";
        return 1;
    }
    init_signals();
    srand(time(NULL));
    const size_t n = rand() % (4 * atoi(argv[1]));
    const string cmd_prefix("../");
    const string cmd_name("readlines-main");
    const string cmd = cmd_prefix + cmd_name;
    const string nstr = std::to_string(n);
    size_t i = 0;
    while (1) {
        int pipe_to_rl[2];
        pipe(pipe_to_rl);
        int test_pid = fork();
        if (!test_pid) {
            dup2(pipe_to_rl[1], 1);
            close_pipe(pipe_to_rl);
            srand(i);
            print_test(n);
            return 0;
        }
        int pipe_from_rl[2];
        pipe(pipe_from_rl);
        int pid = fork();
        if (pid) {
            if (pid == -1) {
                perror("execl failed");
                return -1;
            }
            dup2(pipe_from_rl[0], 0);
            close_pipe(pipe_to_rl);
            close_pipe(pipe_from_rl);
            cout << "Test #" << i << ' ';
            ++i;
            cout << "Waiting for readlines... ";
            cout.flush();
            alarm(pause_time);
            int status;
            if (waitpid(pid, &status, 0) == pid) {
                alarm(0);
                cout << "waiting finished." << endl;
                if (!WIFEXITED(status)) {
                    cerr << "Something went wrong around "
                        << cmd_name << endl;
                    return 100;
                }
                if (WEXITSTATUS(status) != 0) {
                    cerr << "Something went wrong in "
                        << cmd_name << endl;
                    return 101;
                }
                string s;
                cout << "output : " << endl;
                while (!cin.eof()) {
                    cin >> s;
                    if (!cin.eof()) {
                        cout << "    " << s << endl;
                        if (s.length() > n) {
                            cerr << "To long line found: "
                                << s << endl;
                            return 102;
                        }
                    }
                }
                cin.clear();
                cout << "OK" << endl;
            } else {
                alarm(0);
                cerr << "Not fast enough" << endl;
                kill(pid, SIGTERM);
                return 1;
            }
        } else {
            dup2(pipe_to_rl[0], 0);
            dup2(pipe_from_rl[1], 1);
            close_pipe(pipe_to_rl);
            close_pipe(pipe_from_rl);
            execl(cmd.c_str(), cmd_name.c_str(), nstr.c_str(), NULL);
            perror("Error while 'execl' occurred: ");
            return -1;
        }
    }
    return 0;
}

