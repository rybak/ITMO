#include "big_malloc.h"
#include "core.h"

struct big_block
{
    page_type tag;
    size_t size;
}

char * big_malloc(size_t size)
{
    big_block *b = (big_block *) mmap(NULL, size + sizeof(big_block),
        PROT_READ | PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS, -1, 0);
    b->tag = BIG;
    b->size = size;
    return (char *) b;
}

void big_free(void *ptr)
{
    big_block *b = ((big_block *) ptr) - 1;
    munmap(b, b->size + sizeof(big_block));
}

#endif
