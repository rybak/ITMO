#include <iostream>
#include <string>
#include <stdexcept>
#include <unordered_map>
#include <vector>
#include <set>
#include <functional>

#include <cstdlib>

#include "loop.h"
#include "commands.h"


// ~/lab/os-course/midterm/*.c <<< has shell-style sha calculation

void my_exit()
{
    exit(EXIT_SUCCESS);
}

void my_put(const std::string &name)
{
    std::cout << "my_put : " << name << std::endl;
}

void my_delete(const std::string &name)
{
    std::cout << "my_delete : " << name << std::endl;
}

int main(int argc, char* argv[] )
{
    std::set<std::string> commands;
    commands.insert(GET);
    commands.insert(DELETE);
    commands.insert(PUT);
    commands.insert(QUIT);
    while (true)
    {
        std::cout << PROMPT;
        std::string line;
        std::getline(std::cin, line);
        auto words = split_cmd(line);
        if (commands.count(words[0]) == 0)
        {
            std::cout << "Commands:" << std::endl;
            for (auto s : commands)
            {
                std::cout << s << std::endl;
            }
            continue;
        }
        if (words[0] == QUIT)
        {
            return 0;
        }
        if (words.size() != 2)
        {
            continue;
        }
        if (words[0] == PUT)
        {
            my_get(words[1]);
        }
        if (words[0] == DELETE)
        {
            my_delete(words[1]);
        }
    }
    return 0;
}

