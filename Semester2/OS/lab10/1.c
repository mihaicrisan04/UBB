#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>


int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <filename>\n", argv[0]);
        exit(1);
    }

    FILE *file = fopen(argv[1], "r");

    int c2p[2];
    int N;

    fscanf(file, "%d", &N);

    if (pipe(c2p) == -1) {
        perror("pipe");
        exit(1);
    }

    pid_t f = fork();

    if (f == -1) {
        // error on fork
        perror("fork");
        exit(1);
    }
    else if (f == 0) {
        // child
        close(c2p[0]);

        int *arr = (int *)malloc(N * sizeof(int));
        for (int i = 0; i < N; i++) {
            fscanf(file, "%d", &arr[i]);
        }
        fclose(file);

        for (int i = 0; i < N; i++) {
            write(c2p[1], &arr[i], sizeof(int));
        }

        free(arr);
        close(c2p[1]);
        exit(0);
    }
    else {
        // parent
        printf("%d\n", N);
        close(c2p[1]);

        int *arr = (int *)malloc(N * sizeof(int));
        for (int i = 0; i < N; i++) {
            read(c2p[0], &arr[i], sizeof(int));
        } 

        int i = 0;
        int x = 0;
        while (x < N) {
            scanf("%d", &x);
            if (x > N) {
                x = N;
            }
            for (int j = i; j < i + x && j < N; j++) {
                printf("%d\n", arr[j]);
            }
            i += x;
            if (i >= N) {
                break;
            }
        } 

        free(arr);
        wait(NULL);
        close(c2p[0]);
    }

    fclose(file);
    return 0; 
}



