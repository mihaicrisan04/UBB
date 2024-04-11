#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>



int main(int argc, char** argv) {
    srand(time(0));
    int p2c[2], c2p[2];
    if (pipe(p2c) == -1) {
        perror("pipe");
        return 1;
    }
    if (pipe(c2p) == -1) {
        perror("pipe");
        return 1;
    } 

    int f = fork();

    if (f < 0) {
        printf("fork failed\n");
        return 1;
    }
    else if (f == 0) {
        // child
        close(p2c[1]);
        close(c2p[0]);

        // recive array of integers 
        int n = 0;
        if (read(p2c[0], &n, sizeof(int)) == -1) {
            perror("read");
            close(p2c[0]);
            close(c2p[1]);
            exit(1);
        }
    
        int* arr = (int*)malloc(n * sizeof(int));
        for (int i = 0; i < n; i++) {
            if (read(p2c[0], &arr[i], sizeof(int)) == -1) {
                perror("read");
                free(arr);
                close(p2c[0]);
                close(c2p[1]);
                exit(1);
            }
        }

        // calculate sum
        int sum = 0;
        for (int i = 0; i < n; i++) {
            sum += arr[i];
        }
        free(arr);

        // send sum to parent
        if (write(c2p[1], &sum, sizeof(int)) == -1) {
            perror("write");
            close(c2p[1]);
            close(p2c[0]);
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
        int n;
        printf("n=");
        scanf("%d", &n);

        if (n > 0) {
            int* arr = (int*)malloc(n * sizeof(int));

            for (int i = 0; i < n; i++) {
                arr[i] = rand() % 100;
                printf("%d ", arr[i]);
            }
            printf("\n");

            // send n
            if (write(p2c[1], &n, sizeof(int)) == -1) {
                perror("write");
                free(arr);
                close(p2c[1]);
                close(c2p[0]);
                wait(0);
                exit(1);
            }

            // send array
            for (int i = 0; i < n; i++) {
                if (write(p2c[1], &arr[i], sizeof(int)) == -1) {
                    perror("write");
                    free(arr);
                    close(p2c[1]);
                    close(c2p[0]);
                    wait(0);
                    exit(1);
                } 
            }
            free(arr);

            //read sum
            int sum = 0;
            if (read(c2p[0], &sum, sizeof(int)) == -1) {
                perror("read");
                close(p2c[1]);
                close(c2p[0]);
                wait(0);
                exit(1);
            }

            printf("sum=%d\n", sum);
        }
        wait(0);
        close(p2c[1]);
        close(c2p[0]);
    }
    return 0;
}
