#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>


int main(int argc, char* argv[]) {
    int n = 10;
    
    printf("parent pid: %d\n", getpid());
    for (int i = 0; i < 10; i++) {
        pid_t pid = fork();

        if (pid == -1) {
            // error
            perror("fork");
            exit(1);
        }
        else if (pid == 0) {
            // child
            printf("child pid: %d, parent pid: %d\n", getpid(), getppid());
            exit(0);
        }
        else {
            // parent
        }
    }

    for (int i = 0; i < 10; i++) {
        wait(NULL);
    }
    
    return 0;
}

