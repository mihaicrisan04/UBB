#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
// a user-defined header which holds the fifo name so we won't hardcode it
#include "9common.h"

int main(int argc, char *argv[]) {
    int fd, dim, i, k;
    char *buf = (char*)malloc(SIZE*sizeof(char));
    // init allocated space with 0
    memset(buf, 0, SIZE * sizeof(char));
    if((fd = open(myfifo, O_RDONLY)) < 0) {
        perror("Error on open fifo");
        exit(1);
    }

    while(1) {
        // read length of incoming string
        if(read(fd, &dim, sizeof(int)) < 0) {
            perror("Error on read size from fifo");
        }
        // stop condition
        if (dim < 0) {
            break;
        }
        i = 0;
        // keep reading until we read exactly dim bytes
        while(i < dim) {
            int size = (dim - i > SIZE - 1) ? SIZE - 1 : dim - i; 
            if((k = read(fd, buf, size)) < 0) {
                perror("Error on read message from fifo");
            } else {
                i += k;
                printf("%s", buf);
                memset(buf, 0, SIZE * sizeof(char));
            }
        }
        // force flush the output to ensure that data is actually printed
        // this may not be needed if we had a "\n" in our printf
        fflush(stdout);
    }
    // cleanup
    free(buf);
    close(fd);
    return 0;
}
