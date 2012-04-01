unsigned char replicateM_ (unsigned char n, void (*action)()) {
    unsigned char i;
    for (i = 0; i < n; i++) action();
    return i;
}