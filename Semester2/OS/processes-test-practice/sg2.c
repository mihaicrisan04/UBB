#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>

// Problem: for each command line arg create 2 child processes A and B. A will
// compute the length of its arg and send it to parent using its own
// communication channel. B will compute the sum of all digits in the arg and
// send it to the parent using its own communication channel. After all hild
// processes are finished, compute in the parent the average of the results
// from processes A and the sum of rez of child processes B and print themk

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <arg1> <arg2> ... <argN>\n", argv[0]);
        exit(1);
    }
    
    int ma = 0;

    for (int i = 1; i < argc; i++) {
        int a, b;
        int a2b[2], b2a[2];
        if (pipe(a2b) < 0) {
            perror("pipe");
            exit(1);
        }
        if (pipe(b2a) < 0) {
            perror("pipe");
            exit(1);
        }

        a = fork();
        if (a == -1) {
            perror("fork");
            exit(1);
        }
        else if (a == 0) { 
            close(a2b[0]);
            close(b2a[1]);

            int len = strlen(argv[i]);
            if (write(a2b[1], &len, sizeof(len)) < 0) {
                perror("write");
                exit(1);
            }
            printf("Child A sent: %d\n", len);

            close(a2b[1]);
            exit(0);
        }

        b = fork();
        if (b == -1) {
            perror("fork");
            exit(1);
        }
        else if (b == 0) { 
            close(a2b[1]);
            close(b2a[0]);

            int len;
            int sum = 0;

            if (read(a2b[0], &len, sizeof(len)) < 0) {
                perror("read");
                exit(1);
            }
            printf("Child B got: %d\n", len);

            while (len) {
                sum += len % 10;
                len /= 10;
            }

            printf("Child B sent: %d\n", sum);
            if (write(b2a[1], &sum, sizeof(sum)) < 0) {
                perror("write");
                exit(1);
            }

            close(a2b[0]);
            close(b2a[1]);
            exit(0);
        }

        int sum;
        if (read(b2a[0], &sum, sizeof(sum)) < 0) {
            perror("read");
            exit(1);
        }
        printf("Parent got: %d\n", sum);
        ma += sum;
            
        wait(NULL);
        wait(NULL);
    }

    ma /= argc - 1;
    printf("Average: %d\n", ma);

    return 0;
}
