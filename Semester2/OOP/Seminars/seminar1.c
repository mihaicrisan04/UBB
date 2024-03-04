#include <stdlib.h>
#include <stdio.h>

void nothing(int x) {
    printf("The address of x is %p inside the function\n", &x);
    x = x + 1;
}

void increase(int *x) {
    printf("The address of x is %p inside the function\n", x);
    *x = *x + 1;
}

int main() {
    int x = 10;
    nothing(x); 
    increase(&x);
    printf("x = %d\n", x); // x = 11
    printf("The address of x is %p outside the function\n", &x);
    
    // double x[10], y[10];
    // x[0] = 1.0;
    // x[1] = 2.0;
    // y[0] = 3.0;
    // y[1] = 4.0;
    // printf("Manhattan distance: %f\n", manhattan_distance(x, y));

    return 0;
}