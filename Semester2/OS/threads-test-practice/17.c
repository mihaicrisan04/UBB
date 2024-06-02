#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <pthread.h>
#include "pthread_barrier.h"
#include <time.h>

typedef struct {
    int *index;
    int *arr;
    pthread_barrier_t *barrier;
    pthread_mutex_t *mutex;
    int n;
} data;

void *f(void *arg) {
    data *d = (data *)arg;
    int *index = d->index;
    int *arr = d->arr;
    pthread_barrier_t *barrier = d->barrier;
    pthread_mutex_t *mutex = d->mutex;
    int n = d->n;

    while (*index < n) {
        pthread_mutex_lock(mutex);
        arr[*index] = rand() % 100 * 2;
        (*index)++;
        pthread_mutex_unlock(mutex);

        pthread_barrier_wait(barrier);
    } 

    return NULL;
}

void *g(void *arg) {
    data *d = (data *)arg;
    int *index = d->index;
    int *arr = d->arr;
    pthread_barrier_t *barrier = d->barrier;
    pthread_mutex_t *mutex = d->mutex;
    int n = d->n;

    while (*index < n) {
        pthread_mutex_lock(mutex);
        arr[*index] = rand() % 100 * 2 + 1;
        (*index)++;
        pthread_mutex_unlock(mutex);

        pthread_barrier_wait(barrier);
    }

    return NULL;
}

int main(int argc, char *argv[]) {
    srand(time(NULL));
    int n;
    scanf("%d", &n);
    if (n % 2 != 0 || n <= 0) {
        printf("n must be even\n");
        return 1;
    }

    pthread_t th1, th2;
    data arg1, arg2;
    pthread_barrier_t barrier;
    pthread_mutex_t mutex;
    int index = 0;
    int *arr = (int *)malloc(n * sizeof(int));

    pthread_barrier_init(&barrier, NULL, 2);
    pthread_mutex_init(&mutex, NULL);

    arg1.index = &index;
    arg1.arr = arr;
    arg1.n = n;
    arg1.barrier = &barrier;
    arg1.mutex = &mutex;

    arg2.index = &index;
    arg2.arr = arr;
    arg2.n = n;
    arg2.barrier = &barrier;
    arg2.mutex = &mutex;

    pthread_create(&th1, NULL, f, &arg1);
    pthread_create(&th2, NULL, g, &arg2);

    pthread_join(th1, NULL);
    pthread_join(th2, NULL);

    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }

    pthread_barrier_destroy(&barrier);
    pthread_mutex_destroy(&mutex);
    free(arr);

    return 0;
}
