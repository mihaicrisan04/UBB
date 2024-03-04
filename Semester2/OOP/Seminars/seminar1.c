#include <stdlib.h>
#include <stdio.h>

void increase(int *x) {
    printf("The address of x is %p inside the function\n", x);
    *x = *x + 1;
}

int main() {
    int x = 10;
    increase(&x);
    printf("x = %d\n", x); // x = 11
    printf("The address of x is %p outside the function\n", &x);
    return 0;
}