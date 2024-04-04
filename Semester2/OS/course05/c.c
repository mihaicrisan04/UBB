#include <stdio.h>

int main(int argc, char** argv) {
    int i;
    FILE *f;
    
    // popen() opens a process by creating a pipe, forking, and invoking the shell.
    f = popen("less", "w");

    for (i = 99; i >= 0; i--) {
        fprintf(f, "%d\n", i);
    }

    // pclose() closes a stream opened by popen() and waits for the command to terminate.
    pclose(f);
    return 0;
}
