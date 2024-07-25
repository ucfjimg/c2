int main(void)
{
    switch (0) {
	case 1:
here:
	    return 1;
    }

    goto here;
}

