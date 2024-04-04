#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <string.h>


// Lecture notes
// P -> A -> B -> P

int main() {
    int p2a[2], a2b[2], b2p[2], n;
    pipe(p2a);
    pipe(a2b);
    pipe(b2p);

    // pipes are unidirectional
    // p2a[0] - read from parent to A 
    // p2a[1] - write from parent to A

    if (fork() == 0) {
        // A
        close(p2a[1]);     
        close(a2b[0]);
        close(b2p[0]);
        close(b2p[1]);
        // observation: must close the write end of the pipe to detect EOF for
        // read
        // if the write end is not closed, read will block forever in the while
        // loop
        while(1) {
            // read will wait until there is something to read or pipe is closed(no more writers)
            if (read(p2a[0], n, sizeof(int)) < 0) { 
                // error 
                break;
            }    
            if (n <= 0) {
                break;
            }
            n--;
            write(a2b[1], &n, sizeof(int));
        }
        close(p2a[0]);
        close(a2b[1]);
        exit(0);
    }

    if (fork() == 0) {
        // B
        close(p2a[0]);
        close(p2a[1]);
        close(a2b[1]);
        close(b2p[0]);
        while(1) {
            if (read(a2b[0], n, sizeof(int)) < 0) { 
                // error 
                break;
            }    
            if (n <= 0) {
                break;
            }
            n--;
            write(b2p[1], &n, sizeof(int));
        }
        close(a2b[0]);
        close(b2p[1]);
        exit(0);
    }

    // P
    close(a2b[0]);
    close(a2b[1]);
    close(b2p[1]);
    close(p2a[0]);
    n = 7;
    write(p2a[1], &n, sizeof(int));
    while (1) {
        if (read(b2p[0], n, sizeof(int)) < 0) { 
            break;
        }    
        if (n <= 0) {
            break;
        }
        n--;
        write(p2a[1], &n, sizeof(int));
    }
    close(b2p[0]);
    close(p2a[1]);
    wait(0);
    wait(0);
    return 0;
}
