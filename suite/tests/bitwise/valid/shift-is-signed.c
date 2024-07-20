int main(void)
{
    //
    // since integers are signed, this should be -1, not 1
    //
    return (1 << 31) >> 31;
}
