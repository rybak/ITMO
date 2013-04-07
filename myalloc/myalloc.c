
#define SMALL_BLOCK 32
#define BIG_BLOCK 65536

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
void free(void *ptr)
{
    char *block_type = get_block_type(ptr);
    if ((*block_type) == TYPE_SMALL)
    {
        small_free(ptr);
        return;
    }
    if ((*block_type) == TYPE_MEDIUM)
    {
        medium_free(ptr);
        return;
    }
    if ((*block_type) == TYPE_BIG)
    {
        big_free(ptr);
        return;
    }
}
