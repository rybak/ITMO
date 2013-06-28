#include <assert.h>

#define SMALL_BLOCK 32
#define BIG_BLOCK 65536

#include "small_malloc.h"
#include "medium_malloc.h"

void * big_malloc(size_t size);

void * malloc(size_t size)
{
    if (size <= SMALL_BLOCK)
    {
        return (void *) small_malloc(size);
    }
    if (size >= BIG_BLOCK)
    {
        return (void *) big_malloc(size);
    }
    return (void *) medium_malloc(size);
}

#define TYPE_SMALL  's'
#define TYPE_MEDIUM 'm'
#define TYPE_BIG    'b'

char * get_block_type(void *ptr);

void big_free(void *ptr);

void free(void *ptr)
{
    char page_type = get_page_type(ptr);
    switch(page_type)
    {
    case TYPE_MEDIUM:
        medium_free(ptr);
        return;
    case TYPE_SMALL:
        small_free(ptr);
        return;
    case TYPE_BIG:
        big_free(ptr);
        return;
    }
}

#define LOG_PAGE_SIZE 12
#define PAGE_SIZE (1 << LOG_PAGE_SIZE)

char * page_start(void *ptr)
{
    char *p = (char *) ptr;
    return (p >> LOG_PAGE_SIZE) << LOG_PAGE_SIZE;
}

char get_page_type(void *ptr)
{
    return *page_start(ptr);
}

