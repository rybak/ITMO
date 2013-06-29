
#include "small_malloc.h"
#include "medium_malloc.h"
#include "big_malloc.h"
#include "core.h"

extern "C"
{
void * malloc(size_t);
void free(void *);
}

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

void free(void *ptr)
{
    tag page_type = get_page_type(ptr);
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

