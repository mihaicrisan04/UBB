#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/types.h>
#include <time.h>
#include <string.h>

typedef struct {
    char *num;
    int *freq;
    pthread_mutex_t *mutex;
} data;

void *f(void *args) {
    data *d = (data *)args;
    char *num = d->num;
    int *freq = d->freq;
    pthread_mutex_t *mtx = d->mutex;

    for (int i = 0; i < strlen(num); i++) {
        pthread_mutex_lock(mtx);
        freq[num[i] - '0']++;
        pthread_mutex_unlock(mtx);
    }
    return NULL;
}

int main(int argc, char *argv[]) {
    int n = argc - 1;
    
    pthread_t *th = (pthread_t *)malloc(n * sizeof(pthread_t));  
    pthread_mutex_t mtx;
    data *args = (data *)malloc(n * sizeof(data));
    int *freq = (int *)malloc(10 * sizeof(int));

    for (int i = 0; i < 10; i++) {
        freq[i] = 0;
    }

    pthread_mutex_init(&mtx, NULL);

    for (int i = 0; i < n; i++) {
        args[i].num = argv[i + 1];
        args[i].freq = freq;
        args[i].mutex = &mtx;
        pthread_create(&th[i], NULL, f, &args[i]);
    }

    for (int i = 0; i < n; i++) {
        pthread_join(th[i], NULL);
    }

    for (int i = 0; i < 10; i++) {
        printf("%d: %d\n", i, freq[i]);
    }

    free(th);
    free(args);
    free(freq);
    pthread_mutex_destroy(&mtx);
    return 0;
}
