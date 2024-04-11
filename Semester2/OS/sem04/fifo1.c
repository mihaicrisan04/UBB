#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>


int main(int argc, char** argv) {
    srand(time(0));
    
    if (mkfifo("./fifo1", 0600 | O_NCREAT) == -1) {
        perror("Error creating fifo1");
        exit(1);
    }
    if (mkfifo("./fifo2", 0600 | O_NCREAT) == -1) {
        perror("Error creating fifo2");
    
        exit(1);
    }

    int p2c = open("./fifo1", O_WRONLY);
    if (p2c == -1) {
        perror("Error opening fifo1");
        exit(1);
    }
    int c2p = open("./fifo2", O_RDONLY);
    if (c2p == -1) {
        perror("Error opening fifo2");
        exit(1);
    }

    int n;
    printf("n = "); 
    scanf("%d", &n);

    if (n > 0) {


    }

    close(p2c); 
    close(c2p);
    unlink("./fifo1");
    unlink("./fifo2");
    return 0;
}
