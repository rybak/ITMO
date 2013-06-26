#ifndef SMALL_MALLOC_H
#define SMALL_MALLOC_H

void * small_malloc(size_t);
void small_free(void *ptr);

struct small_free_block
{
    struct small_free_block *next;
};

typedef struct small_free_page
{
    struct small_free_page *next_page;
    /* TODO */
} small_free_page;

static small_free_page * small_head = NULL;

char * small_malloc(size_t size)
{
    assert(size <= SMALL_BLOCK);
}

#endif
