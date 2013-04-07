#include <assert.h>

#define SMALL_BLOCK 32
#define BIG_BLOCK 65536

void * small_malloc(size_t size);
void * medium_malloc(size_t size);
void * big_malloc(size_t size);

void * malloc(size_t size)
{
    if (size <= SMALL_BLOCK)
    {
        return small_malloc(size);
    }
    if (size >= BIG_BLOCK)
    {
        return big_malloc(size);
    }
    return medium_malloc(size);
}

#define TYPE_SMALL  's'
#define TYPE_MEDIUM 'm'
#define TYPE_BIG    'b'

char * get_block_type(void *ptr);

void small_free(void *ptr);
void medium_free(void *ptr);
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

char get_page_type(void *ptr)
{
    char *p = (char *) ptr;
    char *page_start = (p >> LOG_PAGE_SIZE) << LOG_PAGE_SIZE;
    return *page_start;
}

typedef struct small_free_page_t
{
    struct small_free_page_t *next_page;
    /* TODO */
} small_free_page_t;

static small_free_page_t * small_head = NULL;

void * small_malloc(size_t size)
{
    assert(size <= SMALL_BLOCK);



}
