int foo(int a, int b);

//
// fine
//
int foo(int a, int b)
{
    return 1;
}

//
// second definition not allowed
//
int foo(int a, int b)
{
    return a + b;
}

int main(void)
{
}
