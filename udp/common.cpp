#include "common.h"

#include <stdio.h>
#include <stdlib.h>

void die(const char *s)
{
    perror(s);
    exit(1);
}

void dontdie(const char *s)
{
    perror(s);
    printf("\ncontinue...\n");
}
