#include <stdio.h>
#include <stdlib.h>
#include <sys/shm.h>
#include <sys/ipc.h>
#include <time.h>


struct absp {
    int a;
    int b;
    int s;
    int p;
};
