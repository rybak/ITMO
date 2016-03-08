#include <stdio.h>
int main(int argc, char *argv[]) {
        int i;
        i = argc;
        int j;
        j = 25;
        if (argc - j > 10)
                j = 10;
        if (i > 5)
                j = 15;
        else
                j = 20;
        printf("%d", j);
        return 0;
}
