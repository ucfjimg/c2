int main(void)
{
    int i;

    for (i = 0; i < 100; i++) {
	if (i == 10) {
	    break;
	}
    }

    return i;
}
