#include "seminar1.h"
// #include <stdlib.h>
// #include <stdio.h>

// void nothing(int x) {
//     printf("The address of x is %p inside the function\n", &x);
//     x = x + 1;
// }

// void increase(int *x) {
//     printf("The address of x is %p inside the function\n", x);
//     *x = *x + 1;
// }

Planet* createPlanet(char *name, char *type, double distance) {
    Planet* planet = malloc(sizeof(Planet));
    strcpy(planet->name, name);
    strcpy(planet->type, type);
    planet->distance = distance;
    return planet;
}

char* getPlanetName(Planet *planet) {
    return planet->name;
}

int main() {
    // int x = 10;
    // nothing(x); 
    // increase(&x);
    // printf("x = %d\n", x); // x = 11
    // printf("The address of x is %p outside the function\n", &x);
    
    // double x[10], y[10];
    // x[0] = 1.0;
    // x[1] = 2.0;
    // y[0] = 3.0;
    // y[1] = 4.0;
    // printf("Manhattan distance: %f\n", manhattan_distance(x, y));

    Planet* x = createPlanet("earth", "terrestrial", 1.0);
    printf("earth = %s\n", getPlanetName(x));
    
    return 0;
}