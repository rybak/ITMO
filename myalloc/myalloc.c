
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
