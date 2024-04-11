#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

int n = 100;

void* fa(void* a) {
    for (int i = 0; i < n; i++) {
        printf("thread a\n");
    }
    return NULL;
}

void* fb(void* a) {
    for (int i = 0; i < n; i++) {
        printf("thread b\n");
    }
    return NULL;
}

int main(int argc, char** argv) {
    int i;
    pthread_t ta, tb;   

    //will call f(NULL); 
    pthread_create(&ta, NULL, fa, NULL);
    pthread_create(&tb, NULL, fb, NULL);
    
    for (i = 0; i < n; i++) {
        printf("main\n");
    }

    pthread_join(ta, NULL);
    pthread_join(tb, NULL);

    return 0;
}
