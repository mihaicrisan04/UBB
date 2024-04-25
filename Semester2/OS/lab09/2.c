#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>


int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <integer>\n", argv[0]);
        exit(1);
    }

    int n = atoi(argv[1]);

    printf("Parent PID: %d\n", getpid());
    for (int i = 0; i < n; i++) {
        pid_t f = fork();
        if (f == -1) {
            // error on fork
            perror("fork");
            exit(1);
        }
        else if (f == 0) {
            // child
            printf("Child PID: %d, PPID: %d\n", getpid(), getppid());
        }
        else {
            // parent
            //printf("Parent PID: %d, Child PID: %d\n", getpid(), f);
            wait(NULL);
            exit(0);
        }
    }
    return 0; 
}



