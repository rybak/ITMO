#ifndef MEDIUM_MALLOC_H
#define MEDIUM_MALLOC_H

char * medium_malloc(size_t);
void medium_free(void *);

struct medium_free_block
{

};

struct medium_block
{
    size_t size;
};

#endif
