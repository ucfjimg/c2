int main(void)
{
    int sum = 0;

    for (int i = 0; i < 10; i++) {
	switch(i % 2) {
	    case 0: continue;
	    case 1: sum += i;
	}
    }

    return sum;
}

