#include <stdio.h>

unsigned char replicateM_ (unsigned char n, int (*action)(int)) {
    unsigned char i;
    for (i = 0; i < n; i++) {
        int j = action(i);
        printf("i = %d, j = %d\n", i, j);
    }
    return i;
}

typedef struct {
    unsigned short x;
    float y;
} pair;
pair mkPair(unsigned short x, float y) {
    return (pair) { x , y };
}