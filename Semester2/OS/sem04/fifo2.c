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
    
    int p2c = open("./fifo1", O_RDONLY);
    if (p2c == -1) {
        perror("Error opening fifo1");
        exit(1);
    }
    int c2p = open("./fifo2", O_WRONLY);
    if (c2p == -1) {
        perror("Error opening fifo2");
        exit(1);
    }


    close(p2c);
    close(c2p);
    return 0;
}
