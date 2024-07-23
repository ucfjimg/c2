int main(void)
{
    int i = 0;
    int j = 1;

loop:
    i = i + 1;
    j = j * 2;
    if (i < 5)
	goto loop;

    return j;
}
