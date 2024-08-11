static long foo = 100;

extern int set_pointer(long *pfoo);

int main(void)
{
    set_pointer(&foo);
}
