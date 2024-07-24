int main(void)
{
    int i;
    int j;

    while (1) {
	goto out;
    }
out:

    i = -5;
    j = 0;

    goto in;
    while (i < 10) {
	j++;
in:
	j++;
	i++;
    }

    return j;
}
