int main(void)
{
    int i;
    int j = 0;

    for (i = 0; i < 3; i++) {
	if (i < 2)
	    continue;
	j++;
    }

    return j;
}
