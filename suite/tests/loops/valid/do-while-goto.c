int main(void)
{
    int i;
    int j;

    do {
	goto out;
    } while(1);
out:

    i = -5;
    j = 0;

    goto in;
    do {
	j++;
in:
	j++;
	i++;
    } while (i < 10);

    return j;
}
