#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <pthread.h>
#include <unistd.h>

int n = 0;
//pthread_mutex_t mutex;
pthread_rwlock_t rwlock;

void* r(void* arg) {
    for (int i = 0; i < 10; i++) {
        pthread_rwlock_rdlock(&rwlock);
        printf("%d\n", n);
        pthread_rwlock_unlock(&rwlock);
    }
    return NULL;
}

void* w(void* arg) {
    for (int i = 0; i < 1000; i++) {
        pthread_rwlock_wrlock(&rwlock);
        n++;
        pthread_rwlock_unlock(&rwlock);
    }
    return NULL;
}

//void* f(void* arg) {
//    for (int i = 0; i < 1000; i++) {
//        pthread_mutex_lock(&mutex);
//        n++;
//        pthread_mutex_unlock(&mutex);
//    }
//    return NULL;
//}

int main(int argc, char *argv[]) {
//  int size = 1000;
//  pthread_t th[size];
//  pthread_mutex_init(&mutex, NULL);

    int r_size = 10;
    int w_size = 2;
    pthread_t r_th[r_size];
    pthread_t w_th[w_size];
    pthread_rwlock_init(&rwlock, NULL);

    for (int i = 0; i < w_size; i++) {
        if (pthread_create(&w_th[i], NULL, w, NULL) != 0) {
            perror("pthread_create");
            return 1;
        }
    }

    for (int i = 0; i < r_size; i++) {
        if (pthread_create(&r_th[i], NULL, r, NULL) != 0) {
            perror("pthread_create");
            return 1;
        }
    }

//  for (int i = 0; i < size; i++) {
//      if (pthread_create(&th[i], NULL, f, NULL) != 0) {
//          perror("pthread_create");
//          return 1;
//      }
//  }

//  for (int i = 0; i < size; i++) {
//      if (pthread_join(th[i], NULL) != 0) {
//          perror("pthread_join");
//          return 1;
//      }
//  }

    for (int i = 0; i < w_size; i++) {
        if (pthread_join(w_th[i], NULL) != 0) {
            perror("pthread_join");
            return 1;
        }
    }

    for (int i = 0; i < r_size; i++) {
        if (pthread_join(r_th[i], NULL) != 0) {
            perror("pthread_join");
            return 1;
        }
    }

    printf("n = %d\n", n);

//  pthread_mutex_destroy(&mutex);
    pthread_rwlock_destroy(&rwlock);
    return 0;
}
