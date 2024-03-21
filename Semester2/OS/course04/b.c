#include <stdio.h>
#include <unistd.h>
#include <stdlib.h> 
#include <sys/wait.h>
#include <sys/types.h>


int main(int argc, char** argv) {
    int pid = fork();
    if (pid == 0) {
        printf("child pid = %d; fork return = %d\n", getpid(), pid);
        exit(0);
    }
    printf("parent pid =  %d\n", pid);
    wait(0); // waits for the first child to finish
    return 0;
}

