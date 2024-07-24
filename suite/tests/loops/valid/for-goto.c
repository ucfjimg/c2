int main(void)
{
    int i;
    int j;

    for (;;) {
	goto out;
    }
out:

    i = -5;
    j = 0;
    goto in;
    for (i = 0; i < 10; i++) {
    	in:
	    j++;
    }

    return j;
}
