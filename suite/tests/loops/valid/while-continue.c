int main(void)
{
   int i = 0;
   int j = 0;

   while (i < 100) {
	i++;
	if (i < 10) {
	    continue;
	}
	j++;
    }

    return j;
}
