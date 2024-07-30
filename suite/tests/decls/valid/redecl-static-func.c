static int foo(void);
extern int foo(void);
static int foo(void)
{
    return 1;
}

int main(void)
{
    return foo();
}

