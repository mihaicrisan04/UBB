#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

int n = 100;

void* f(void* a) {
    for (int i = 0; i < n; i++) {
        printf("thread %s\n", (char*)a);
    }
    return NULL;
}


int main(int argc, char** argv) {
    int i;
    pthread_t ta, tb;   

    //will call f(NULL); 
    pthread_create(&ta, NULL, f, "a");
    pthread_create(&tb, NULL, f, "b");
    
    for (i = 0; i < n; i++) {
        printf("main\n");
    }

    pthread_join(ta, NULL);
    pthread_join(tb, NULL);

    return 0;
}
