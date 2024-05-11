#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <string.h>

void f(int n) {
    if (n > 0) {
        int k = fork();
        if (k == -1) {
            perror("fork");
            exit(1);
        }
        else if (k == 0) {
            printf("child pid: %d, parent pid: %d\n", getpid(), getppid());
            f(n - 1);
        }
    }
    wait(NULL);
    exit(0);
}

int main(int argc, char** argv) {
    int n = 10;
        
    printf("parent pid: %d\n", getpid());
    // recursive
    f(n);
    

    // iterative
    printf("\nparent pid: %d\n", getpid());
    for (int i = 0; i < n; i++) {
        pid_t pid = fork();
        if (pid == -1) {
            perror("fork");
            exit(1);
        }
        else if (pid == 0) {
            printf("child pid: %d, parent pid: %d\n", getpid(), getppid());
        }
        else {
            wait(NULL);
            exit(0);
        }
    }
    return 0;
}
