#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>
#include <math.h>
#include <errno.h>


int main() {
    srand(time(NULL));

    int p2b[2];
    if (pipe(p2b) == -1) {
        perror("pipe");
        exit(1);
    }

    int b = fork();
    if (b == -1) {
        perror("fork");
        exit(1);
    }
    else if (b == 0) {
        close(p2b[1]);
        int n = rand() % 901 + 100;
        printf("Process B has generated %d\n", n);
        int num;
        while (read(p2b[0], &num, sizeof(num))) {
            int dif = (n - num < 0) ? num - n : n - num;
            printf("B recieved %d; difference: %d\n", num, dif);
            if (dif < 50) {
                break;
            }
        }

        close(p2b[0]);
        exit(0);
    }

    close(p2b[0]);

    int k = 0;
    while (1) {
        int num = rand() % 1001 + 50;
        k++;
        int result = write(p2b[1], &num, sizeof(num));
        if (result == EPIPE) {
            break;
        }
        else if (result == -1) {
            perror("write");
            break;
        }
    }


    printf("Process A has generated %d numbers\n", k);

    close(p2b[1]);

    wait(NULL);
    return 0;
}
