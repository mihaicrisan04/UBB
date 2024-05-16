// create n threads
// on startup each trhead generates a value and stores it in a shread array
// after each thread has generated its value, each thread attempts to steal 10%
// from another thread's value

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>

pthread_barrier_t bar;
pthread_mutex_t mutex;
int n;
int* arr;

void *f(void *arg) {
    int index = *((int*)arg);
    printf("before barrier\n");

    arr[index] = rand() % 1000;

    printf("before barrier - id: %d, val: %d\n", index, arr[index]);

    pthread_barrier_wait(&bar);

    pthread_mutex_lock(&mutex);

    int other_index = rand() % n;
    arr[index] += arr[other_index] / 10;
    arr[other_index] -= arr[other_index] / 10;

    printf("Thread %d stole 10%% from thread %d\n", index, other_index);

    pthread_mutex_unlock(&mutex);   

    return NULL;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <n>\n", argv[0]);
        return 1;
    }
    srand(time(NULL));

    n = atoi(argv[1]); 
    arr = malloc(n * sizeof(int));
    int *indexes = malloc(n * sizeof(int));
    pthread_t th[n];
    pthread_mutex_init(&mutex, NULL);
    pthread_barrier_init(&bar, NULL, n);
    for (int i = 0; i < n; i++) {
        indexes[i] = i;
        if (pthread_create(&th[i], NULL, f, &indexes[i]) != 0) {
            perror("pthread_create");
            return 1;
        }
    }

    for (int i = 0; i < n; i++) {
        if (pthread_join(th[i], NULL) != 0) {
            perror("pthread_join");
            return 1;
        }
    }

    pthread_barrier_destroy(&bar);
    pthread_mutex_destroy(&mutex);
    free(indexes);
    free(arr);
    return 0;
}
