int main(void)
{
    //
    // (2 < 1) < 1 => 0 < 1 => 1
    // wrong would be 2 < (1 < 1) => 2 < 0 => 0
    // 
    return 2 < 1 < 1;
}
