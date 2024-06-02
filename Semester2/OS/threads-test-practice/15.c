#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include "pthread_barrier.h"
#include <string.h>

typedef struct {
    char *str;
    int *digits;
    int *letters;
    int *puncs;
} data;

pthread_mutex_t mtx;

void *count(void *args) {
    data arg = *((data *)args);
    char *str = arg.str;
    int *digits = arg.digits;
    int *letters = arg.letters;
    int *puncs = arg.puncs;

    for (int i = 0; i < strlen(str); i++) {
        if (str[i] >= '0' && str[i] <= '9') {
            pthread_mutex_lock(&mtx);
            (*digits)++;
            pthread_mutex_unlock(&mtx);
        } else if ((str[i] >= 'a' && str[i] <= 'z') || (str[i] >= 'A' && str[i] <= 'Z')) {
            pthread_mutex_lock(&mtx);
            (*letters)++;
            pthread_mutex_unlock(&mtx);
        } else {
            pthread_mutex_lock(&mtx);
            (*puncs)++;
            pthread_mutex_unlock(&mtx);
        }
    }
    return NULL;
}


int main(int argc, char *argv[]) {
    int n = argc - 1;
    char **strs = (char **)malloc(n * sizeof(char *));
    int digits = 0, letters = 0, puncs = 0;

    pthread_t *th = (pthread_t *)malloc(n * sizeof(pthread_t));
    data *args = (data *)malloc(n * sizeof(data));

    for (int i = 1; i <= n; i++) {
        strs[i - 1] = (char *)malloc(strlen(argv[i] + 1) * sizeof(char));
        strcpy(strs[i - 1], argv[i]); 
        strs[i - 1][strlen(argv[i])] = '\0';
    }

    pthread_mutex_init(&mtx, NULL);

    for (int i = 0; i < n; i++) {
        args[i].str = strs[i];
        args[i].digits = &digits;
        args[i].letters = &letters;
        args[i].puncs = &puncs;
        pthread_create(&th[i], NULL, count, &args[i]);
    } 

    for (int i = 0; i < n; i++) {
        pthread_join(th[i], NULL);
    }

    printf("Total digits: %d\n", digits);
    printf("Total letters: %d\n", letters);
    printf("Total punctuation: %d\n", puncs);

    pthread_mutex_destroy(&mtx);

    for (int i = 0; i < n; i++) {
        free(strs[i]);
    }
    free(strs);
    free(args);
    free(th);
    return 0;
}


