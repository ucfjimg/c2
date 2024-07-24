int main(void)
{
    int i = 0;
    int j = 0;

    do {
	i++;
	if (i < 10)
	    continue;
	j++;
    } while (i < 100);

    return j;
}
