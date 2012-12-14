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

const int SYS_ERROR = 1,
          USAGE_ERROR = 2,
          LONG_ERROR = 100,
          SLOW_ERROR = 101,
          WAIT_ERROR = 200,
          NOT_EXITED_ERROR = 201,
          RL_ERROR   = 202;

const size_t pause_time = 5;
const char pause_mark = '@';

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
    bool pause_been = false;
    const size_t test_size = 10;
    for (size_t t = 0; t < test_size; ++t) {
        size_t len = rand() % (2 * n);
        string s = rand_str(len);
        if (len > 1) {
            bool with_pause = ((rand() % pause_prob == 0) && !pause_been);
            size_t pause_pos = rand() % (len - 1) + 1;
            if (with_pause) {
                s[pause_pos - 1] = 'P';
                s[len - 1] = pause_mark;
                cout << s.substr(0, pause_pos);
                pause_been = true;
                make_pause();
                cout << s.substr(pause_pos, len - pause_pos);
            } else {
                cout << s;
            }
        }
        cout << endl;
    }
    close(1);
}

void close_pipe(int fd[]) {
    close(fd[0]);
    close(fd[1]);
}

bool pause_happened = false;

void alarm_handler(int signum) {
    pause_happened = true;
}

void init_signals() {
    struct sigaction new_action;
    if (sigemptyset(&new_action.sa_mask)) {
        perror("Can't use sigemptyset.");
        exit(SYS_ERROR);
    }
    new_action.sa_handler = alarm_handler;
    if (sigaction(SIGALRM, &new_action, NULL)) {
        perror("Can't use sigaction");
        exit(SYS_ERROR);
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        cout << "Usage: tester K\n";
        return USAGE_ERROR;
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
                return SYS_ERROR;
            }
            dup2(pipe_from_rl[0], 0);
            close_pipe(pipe_to_rl);
            close_pipe(pipe_from_rl);
            cout << "Test #" << i << ' ';
            ++i;
            cout << "Waiting for readlines...\noutput:" << endl;
            while (!cin.eof()) {
                string s;
                alarm(pause_time + 1);
                cin >> s;
                alarm(0);
                if (!cin.eof()) {
                    cout << "    " << s << endl;
                    if (s.length() > n) {
                        cerr << "To long line found: "
                            << s << endl;
                        return LONG_ERROR;
                    }
                } else {
                    break;
                }
                if (pause_happened) {
                    pause_happened = false;
                    if (s[s.length()] != pause_mark) {
                        cerr << "To slow" << endl;
                        kill(pid, SIGTERM);
                        return SLOW_ERROR;
                    }
                }
            }
            int status;
            if (waitpid(pid, &status, 0) == pid) {
                cout << "waiting finished." << endl;
                if (!WIFEXITED(status)) {
                    cerr << "Something went wrong around "
                        << cmd_name << endl;
                    return NOT_EXITED_ERROR;
                }
                if (WEXITSTATUS(status) != 0) {
                    cerr << "Something went wrong in "
                        << cmd_name << endl;
                    return RL_ERROR;
                }
            } else {
                cerr << "Oops" << endl;
                return WAIT_ERROR;
            }
            cin.clear();
            cout << "OK" << endl;
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

