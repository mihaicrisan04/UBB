#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/types.h>

typedef struct {
    int id;
    int m;
    int *arr;
} data;

void *f(void *arg) {
    data *d = (data *)arg;
    int id = d->id;
    int m = d->n;
    int *arr = d->arr;

    return NULL;
}

int main(int argc, char *argv[]) {
    int n = argc - 1;
    int m = 1;
    while (m < n) {
        m *= 2;
    }
    int arr[m];
    for (int i = 0; i < n; i++) {
        arr[i] = atoi(argv[i + 1]);
    }

    pthread_t th[m];
    data d[m];
    for (int i = 0; i < m - 1; i++) {
        d[i].id = i;
        d[i].m = m;
        d[i].arr = arr;
        pthread_create(&th[i], NULL, f, &d[i]);
    }


    return 0;
}
