/*
 * Write two C programs that communicate via fifo. Program A is responsible for creating/deleting the fifo. 
 * Program A reads commands from the standard input, executes them and sends the output to program B. Program B 
 * keeps reading from the fifo and displays whatever it receives at the standard output. This continues until 
 * program A receives the "stop" command.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
// a user-defined header that contains the fifo name so we won't hardcode it
#include "9common.h"

void make_fifo_if_not_exists(char * fifo_name) {
    // maybe check if fifo already exists and remove it
    // the following functions could help
    // stat
    // unlink
    if(mkfifo(fifo_name, 0600) < 0) {
        perror("Error on create fifo");
        exit(1);
    }
}

void write_to_fifo(int fd, char *buf) {
    // function that sends first the length of the string and then the string itself
    int nr = strlen(buf);
    if(0 > write(fd, &nr, sizeof(int))) {
        perror("Error on write size to fifo");
    }
    if(0 > write(fd, buf, nr * sizeof(char))) {
        perror("Error on write message to fifo");
    }
}

void handler(int sig) {
    // in case we will Ctrl+C this program, have a handler to cleanup
    printf("\nSIGINT received; cleaning up\n");
    if(unlink(myfifo) <0){
        perror("Unlink failed");
        exit(1);
    }
    exit(0);
}

int main(int argc, char *argv[]) {
    int fd, k;
    FILE *f;
    char *cmd = (char*) malloc(SIZE * sizeof(char));
    memset(cmd, 0, SIZE * sizeof(char));
    char *buf = (char*) malloc(SIZE * sizeof(char));
    memset(buf, 0, SIZE * sizeof(char));

    signal(SIGINT, handler);
    
    make_fifo_if_not_exists(myfifo);
    if((fd = open(myfifo, O_WRONLY)) < 0) {
        perror("Error on open fifo");
        exit(1);
    }

    while(1) {
        // read commands from stdin (alias 0), this way we get whitespaces too
        if((k = read(0, cmd, SIZE*sizeof(char))) < 0) {
            perror("Error reading command");
        }
        // ensure NULL terminated string
        cmd[k-1] = 0;
        if(strcmp(cmd, "stop") == 0) {
            printf("Exiting...\n");
            break;
        }
        // execute the command
        if((f = popen(cmd, "r")) == NULL) {
            perror("Error on popen");
        } else {
            // keep reading the output of the command until done
            while(fread(buf, 1, SIZE-1, f) > 0) {
                // send chunks to the other process
                write_to_fifo(fd, buf);
                memset(buf, 0, SIZE * sizeof(char));
            }
        }
        // cleanup
        pclose(f);
        memset(cmd, 0, SIZE * sizeof(char));
    }
    int stop = -1;
    // send message to stop
    if(0 > write(fd, &stop, sizeof(int))) {
        perror("Error on write size to fifo");
    }
    // free resources
    close(fd);
    free(cmd);
    free(buf);
    if(unlink(myfifo) <0){
        perror("Unlink failed");
        exit(1);
    }    
    return 0;
}
