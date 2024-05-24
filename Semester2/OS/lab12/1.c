#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "pthread_barrier.h"


int *arr;
pthread_mutex_t mutex;
pthread_barrier_t barrier;

void *thread0(void *arg) {
    int sum = 0;
    pthread_mutex_lock(&mutex);
    for (int i = 0; i < 10; i++) {
        arr[i] = rand() % 101;
        printf("%4d", arr[i]);
        sum += arr[i];
    } 
    pthread_mutex_unlock(&mutex);
    printf("\nThread 0 sum: %d\n", sum);
    pthread_barrier_wait(&barrier);
    return NULL;
}

void *thread1(void *arg) {
    int sum = 0;
    pthread_barrier_wait(&barrier);
    for (int i = 0; i < 10; i++) {
        pthread_mutex_lock(&mutex);
        if (arr[i]) {
            sum += arr[i];
            arr[i] = 0;
        }
        pthread_mutex_unlock(&mutex);
        usleep(100000);
    }
    printf("Thread 1: %d\n", sum);
    return NULL;
}

void *thread2(void *arg) {
    int sum = 0;
    pthread_barrier_wait(&barrier);
    for (int i = 0; i < 10; i++) {
        pthread_mutex_lock(&mutex);
        if (arr[i]) {
            sum += arr[i];
            arr[i] = 0;
        }
        pthread_mutex_unlock(&mutex);
        usleep(100000);
    }
    printf("Thread 2: %d\n", sum);
    return NULL;
}

int main(int argc, char *argv[]) {
    srand(time(NULL));
    int n = atoi(argv[1]);
    pthread_t threads[3];
    arr = (int*)malloc(n * sizeof(int));
    pthread_mutex_init(&mutex, NULL);
    pthread_barrier_init(&barrier, NULL, 3);

    for (int i = 0; i < n; i++) {
        pthread_create(&threads[0], NULL, thread0, NULL);
        pthread_create(&threads[1], NULL, thread1, NULL);
        pthread_create(&threads[2], NULL, thread2, NULL);

        pthread_join(threads[0], NULL);
        pthread_join(threads[1], NULL);
        pthread_join(threads[2], NULL);

        printf("\n");
    }
    
    pthread_mutex_destroy(&mutex);
    pthread_barrier_destroy(&barrier);
    free(arr);
    return 0;
}
