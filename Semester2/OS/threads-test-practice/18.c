#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>


typedef struct {
    char *s;
} data;

void *f(void *arg) {
    data *d = (data *) arg;
    char *s = d->s;
    for (int i = 0; i < strlen(s); i++) {
        if (s[i] >= 'a' && s[i] <= 'z') {
            s[i] -= 32;
        }
    }
    return NULL;
}

int main(int argc, char *argv[]) {
    int n = argc - 1;
    pthread_t th[n];
    data args[n];
    for (int i = 0; i < n; i++) {
        args[i].s = argv[i + 1];
        pthread_create(&th[i], NULL, f, &args[i]);
    }

    for (int i = 0; i < n; i++) {
        pthread_join(th[i], NULL);
    }

    for (int i = 0; i < n; i++) {
        printf("%s ", args[i].s);
    }
    return 0;
}
