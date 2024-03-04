#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char name[50];
    char type[50];
    double distance;
} Planet;

char *getPlanetName(Planet *planet);

Planet *createPlanet(char *name, char *type, double distance);