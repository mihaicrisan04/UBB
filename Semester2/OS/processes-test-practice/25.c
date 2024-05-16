#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>


int main() {
    int a2d[2], b2d[2], c2d[2];
    pipe(a2d), pipe(b2d), pipe(c2d);

    int n = rand() % 11 + 10;
    write(a2d[1], &n, sizeof(int));
    printf("A sent to D %d\n", n);
    close(a2d[1]);

    int b = fork();
    if (b == 0) {
        srand(getpid());

        close(a2d[0]);
        close(c2d[0]);
        close(c2d[1]);
        close(b2d[0]);

        while (1) {
            int num = rand() % 201 + 1;
            int res = write(b2d[1], &num, sizeof(int));
            sleep(1);
            printf("B sent %d\n", num);
            if (res < 0) {
                break;
            }
        }

        close(b2d[1]);
        exit(0);
    }

    int c = fork();
    if (c == 0) {
        srand(getpid());

        close(a2d[0]);
        close(b2d[0]);
        close(b2d[1]);
        close(c2d[0]);

        while (1) {
            int num = rand() % 201 + 1;
            int res = write(c2d[1], &num, sizeof(int));
            sleep(1);
            printf("C sent %d\n", num);
            if (res < 0) {
                break;
            }
        }

        close(c2d[1]);
        exit(0);
    }

    int d = fork();
    if (d == 0) {
        close(b2d[1]);
        close(c2d[1]);

        int na;
        read(a2d[0], &na, sizeof(int));
        printf("D got from A %d\n", na);

        while (1) {
            int nb, nc;
            read(b2d[0], &nb, sizeof(int));
            read(c2d[0], &nc, sizeof(int));
            printf("D got from B %d\n", nb);
            printf("D got from C %d\n", nc);
            if (abs(nb - nc) < na) {
                printf("dif %d stopping\n", abs(nb - nc));
                break;
            }
        }

        close(a2d[0]);
        close(b2d[0]);
        close(c2d[0]);
        exit(0);
    }

    close(a2d[1]);
    close(b2d[0]);
    close(b2d[1]);
    close(c2d[0]);
    close(c2d[1]);

    wait(NULL);
    wait(NULL);
    return 0;
}
