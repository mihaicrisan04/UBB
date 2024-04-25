#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <pthread.h>

typedef struct {
    int id;
    char *s;
} data;

void* upcase(void *arg) {
    // cast the argument to the corect type and dereference it
    data d = *(data*)arg;
    char* copy = malloc(sizeof(char) * (strlen(d.s) + 1));
    strcpy(copy, d.s);
    for (int i = 0; i < strlen(d.s); i++) {
        copy[i] = toupper(d.s[i]);
    }
    printf("Thread %d: arg %s - result %s\n", d.id, d.s, copy); 
    return copy;
}

int main(int argc, char *argv[]) {
    int size = argc - 1;
    pthread_t th[size];
    data *args = malloc(size * sizeof(data));

    for (int i = 0; i < size; i++) {
        args[i].id = i;
        args[i].s = argv[i + 1];
        if (pthread_create(&th[i], NULL, upcase, &args[i]) != 0) {
            perror("pthread_create");
            return 1;
        }
    }

    for (int i = 0; i < size; i++) {
        void* res;
        if (pthread_join(th[i], &res) != 0) {
            perror("pthread_join");
            return 1;
        }
        char* str = (char*)res;
        printf("Thread %d: result %s\n", i, str);
        free(str);
    }

    printf("\n");
    for (int i = 0; i < size; i++) {
        printf("arg %d: %s\n", i, args[i].s);
    }

    free(args);
    return 0;
}
