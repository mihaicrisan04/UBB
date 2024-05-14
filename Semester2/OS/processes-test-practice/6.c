#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>


int main(int argc, char *argv[]) { 
    if (argc != 2) {
        printf("Usage: %s <command>\n", argv[0]);
        exit(1);
    }
    int p2c[2], c2p[2];

    if (pipe(p2c) == -1) {
        perror("pipe");
        exit(1);
    }
    if (pipe(c2p) == -1) {
        perror("pipe");
        exit(1);
    }

    int f = fork();

    if (f == -1) {
        perror("fork");
        exit(1);
    }
    else if (f == 0) {
        // child
        close(p2c[1]);
        close(c2p[0]);

        int n;
        if (read(p2c[0], &n, sizeof(n)) == -1) {
            perror("read");
            exit(1);
        }
        int list[n];
        if (read(p2c[0], list, sizeof(list)) == -1) {
            perror("read");
            exit(1);
        }

        int sum = 0;
        for (int i = 0; i < n; i++) {
            sum += list[i];
        }

        printf("Child:\n");
        for (int i = 0; i < n; i++) {
            printf("%d ", list[i]);
        }
        printf("\nsum: %d - ma: %d\n", sum, sum / n);

        sum /= n;

        if (write(c2p[1], &sum, sizeof(sum)) == -1) {
            perror("write");
            exit(1);
        }

        close(p2c[0]);
        close(c2p[1]);
        exit(0);
    }
    else {
        // parent
        close(p2c[0]);
        close(c2p[1]);
        srand(time(NULL));

        int n = atoi(argv[1]);
        int list[n];
        for (int i = 0; i < n; i++) {
            list[i] = rand() % 100;
        }

        if (write(p2c[1], &n, sizeof(n)) == -1) {
            perror("write");
            exit(1);
        }
        if (write(p2c[1], list, sizeof(list)) == -1) {
            perror("write");
            exit(1);
        }

        if (read(c2p[0], &n, sizeof(n)) == -1) {
            perror("read");
            exit(1);
        }
        printf("Parent: %d\n", n);


        close(p2c[1]);
        close(c2p[0]);
        wait(NULL);
    }

    return 0;
}

