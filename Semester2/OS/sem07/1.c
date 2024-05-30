#include <stdio.h>
#include <stdlib.h> 
#include <pthread.h>
#include <string.h>
#include <unistd.h>

// write a c program that takes 2 numbers, n and m, as arguments from the
// command line.
// the program creates n "generator" threads that generate random lowercase
// letters and appends them to a string with 128 positions. the program will
// create an additional "printer" thread that waits until all the positions of
// the stirng are fillles, at which point it prints the string and clears it .
// the n "generator" threads must generate a total o m such strings and the
// "printer" thread prints each one as soon as it gets to length 128

#define SIZE 50

typedef struct {
    int id;
    char *buf;
    int *pos;
    int maxSize;
    int *repeats;
    pthread_mutex_t *m;
    pthread_cond_t *c;
} data;

void *gen(void *arg) {
    data d = *((data *) arg);
    
    while (1) {
        pthread_mutex_lock(d.m);
        if (*(d.repeats) == 0) {
            pthread_cond_broadcast(d.c);
            pthread_mutex_unlock(d.m);
            break;
        }
        while (*(d.pos) == d.maxSize) {
            pthread_cond_broadcast(d.c);
            pthread_cond_wait(d.c, d.m);
        }
        char c = rand() % 26 + 'a';
        d.buf[*(d.pos)] = c;
        *(d.pos) += 1;
        pthread_mutex_unlock(d.m);   
    }
    return NULL;
}

void *printer(void *arg) {
    data d = *((data *) arg);
    
    while (1) {
        pthread_mutex_lock(d.m);
        if (*(d.repeats) == 0) {
            pthread_cond_broadcast(d.c);
            pthread_mutex_unlock(d.m);
            break;
        }
        while (*(d.pos) < d.maxSize) {
            pthread_cond_broadcast(d.c);
            pthread_cond_wait(d.c, d.m);
        }
        printf("%s\n", d.buf);
        memset(d.buf, 0, d.maxSize);
        *(d.pos) = 0;
        *(d.repeats) -= 1;
        pthread_mutex_unlock(d.m);   
    }
    return NULL;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Usage: %s n m\n", argv[0]);
        return 1;
    }
    srand(time(NULL));

    int n = atoi(argv[1]);
    int m = atoi(argv[2]);
    int pos = 0;
    pthread_mutex_t mtx;
    pthread_cond_t cond;
    pthread_mutex_init(&mtx, NULL);
    pthread_cond_init(&cond, NULL);
    pthread_t th[n+1];
    data args[n+1];
    char *buf = malloc((SIZE + 1) * sizeof(char));
    memset(buf, 0, SIZE);
    for (int i = 0; i < n; i++) {
        args[i].id = i;
        args[i].c = &cond;
        args[i].m = &mtx;
        args[i].maxSize = SIZE;
        args[i].pos = &pos;
        args[i].repeats = &m;
        args[i].buf = buf;
        pthread_create(&th[i], NULL, gen, &args[i]);
    }

    args[n].id = n;
    args[n].c = &cond;
    args[n].m = &mtx;
    args[n].maxSize = SIZE;
    args[n].pos = &pos;
    args[n].repeats = &m;
    args[n].buf = buf;
    pthread_create(&th[n], NULL, printer, &args[n]);

    for (int i = 0; i < n; i++) {
        pthread_join(th[i], NULL);
    }
    pthread_join(th[n], NULL);
  
    pthread_mutex_destroy(&mtx);
    pthread_cond_destroy(&cond);
    free(buf);
    return 0;
}

