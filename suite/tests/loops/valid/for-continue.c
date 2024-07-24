int main(void)
{
    int i;
    int j = 0;

    for (i = 0; i < 100; i++) {
	if (i < 10)
	    continue;
	j++;
    }

    return j;
}
