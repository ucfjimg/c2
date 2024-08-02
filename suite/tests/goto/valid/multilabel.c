int foo(void)
{
label:  goto label;
}

int main(void)
{
    goto label;
label:
    return 0;
}

