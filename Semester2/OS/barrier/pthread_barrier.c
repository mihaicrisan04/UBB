#include "pthread_barrier.h"

int pthread_barrier_init(pthread_barrier_t *barrier, const pthread_barrierattr_t *attr, unsigned int count) {
    if (count == 0) return EINVAL;

    barrier->count = count;
    barrier->left = count;

    pthread_mutex_init(&barrier->mutex, NULL);
    pthread_cond_init(&barrier->cond, NULL);

    return 0;
}

int pthread_barrier_wait(pthread_barrier_t *barrier) {
    pthread_mutex_lock(&barrier->mutex);
    barrier->left--;

    if (barrier->left == 0) {
        barrier->left = barrier->count;
        pthread_cond_broadcast(&barrier->cond);
        pthread_mutex_unlock(&barrier->mutex);
        return PTHREAD_BARRIER_SERIAL_THREAD;
    } else {
        pthread_cond_wait(&barrier->cond, &barrier->mutex);
        pthread_mutex_unlock(&barrier->mutex);
        return 0;
    }
}

int pthread_barrier_destroy(pthread_barrier_t *barrier) {
    pthread_mutex_destroy(&barrier->mutex);
    pthread_cond_destroy(&barrier->cond);
    return 0;
}

