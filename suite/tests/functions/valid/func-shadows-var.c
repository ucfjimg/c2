int main(void)
{
    int foo = 4;
    int bar;

    bar = foo + 4;
    {
        int foo(int a, int b);
        bar = foo(bar, bar);
    }
    
    return bar;
}

int foo(int a, int b)
{
    return a + b;
}
