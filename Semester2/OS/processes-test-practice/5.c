#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <time.h>

int is_boltz(int n) {
    if (n % 7 == 0) return 1;
    while (n) {
        if (n % 10 == 7) return 1;
        n /= 10;
    }
    return 0;
}

int main(int argc, char **argv) {
    srand(time(NULL));
    int n = 5;
    int p2p[n][2];

    for (int i = 0; i < n; i++) {
        if (pipe(p2p[i]) < 0) {
            perror("pipe");
            exit(1);
        }
    }

    for (int i = 0; i < n; i++) {
        pid_t f = fork();
        if (f == -1) {
            perror("fork");
            exit(1);
        }
        else if (f == 0) {
            // child
            
        }
        else { 
            // parent
            
        }
    }

    return 0;
}
