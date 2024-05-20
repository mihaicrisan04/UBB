#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
//#include "pthread_barrier.h"
#include <time.h>

int n, m;
int **a;
int *sums;

void *row_sum(void *arg) {
    int row = (int)arg;
    int sum = 0;
    for (int i = 0; i < m; i++) {
        sum += a[row][i];
    }
    sums[row] = sum;
    printf("Row sum: %d\n", sum);
    return NULL;
}


int main(int argc, char *argv[]) {
    FILE *f = fopen("12.in", "r");

    pthread_t *threads;

    fscanf(f, "%d %d", &n, &m);
    a = (int **)malloc(n * sizeof(int *));
    for (int i = 0; i < n; i++) {
        a[i] = (int *)malloc(m * sizeof(int));
        for (int j = 0; j < m; j++) {
            fscanf(f, "%d", &a[i][j]);
        }
    }

    threads = (pthread_t *)malloc(n * sizeof(pthread_t));
    sums = (int *)malloc(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        pthread_create(&threads[i], NULL, row_sum, (void *)i);
    }

    for (int i = 0; i < n; i++) {
        pthread_join(threads[i], NULL);
    }
    free(threads);

    int sum = 0; 
    for (int i = 0; i < n; i++) {
        sum += sums[i];
    }

    printf("Total sum: %d\n", sum);

    for (int i = 0; i < n; i++) {
        free(a[i]);
    }
    free(a);
    free(sums);
    fclose(f);
    return 0;
}
