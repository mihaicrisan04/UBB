#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <time.h>


int main(int argc, char *argv[]) {
    printf("Parent PID: %d\n", getpid());
    int c1_2p[2], c2_2p[2];
    int p2c1[2], p2c2[2];

    if (pipe(c1_2p) == -1) {
        perror("pipe");
    }
    if (pipe(c2_2p) == -1) {
        perror("pipe");
    }
    if (pipe(p2c1) == -1) {
        perror("pipe");
    }
    if (pipe(p2c2) == -1) {
        perror("pipe");
    }

    int f1 = fork();
    if (f1 == -1) {
        perror("fork");    
    }
    else if (f1 == 0) {
        close(c2_2p[0]);
        close(c2_2p[1]);
        close(c1_2p[0]);
        
        srand(getpid());
        int n1 = rand() % 1000;
        printf("Child PID: %d, Random number: %d\n", getpid(), n1);
        
        if (write(c1_2p[1], &n1, sizeof(n1)) == -1) {
            perror("write c1");
            close(c1_2p[1]);
            exit(1);
        }
        if (read(p2c1[0], &n1, sizeof(n1)) == -1) {
            perror("read p2c1");
            close(c1_2p[1]);
            exit(1);
        }

        if (n1 == 1) {
            printf("Child PID: %d, I am the winner\n", getpid());
        }
        else if (n1 == -1) {
            printf("Child PID: %d, I am the loser\n", getpid());
        }
        else {
            printf("Child PID: %d, It's a tie\n", getpid());
        }
        close(c1_2p[1]);
        
        exit(0);
    }

    int f2 = fork();
    if (f2 == -1) {
        perror("fork"); 
    }
    else if (f2 == 0) {
        close(c1_2p[0]);
        close(c1_2p[1]);
        close(c2_2p[0]);

        srand(getpid());
        int n2 = rand() % 1000;
        printf("Child PID: %d, Random number: %d\n", getpid(), n2);

        if (write(c2_2p[1], &n2, sizeof(n2)) == -1) {
            perror("write c2");
            close(c2_2p[1]);
            exit(1);
        }
        if (read(p2c2[0], &n2, sizeof(n2)) == -1) {
            perror("read p2c2");
            close(c2_2p[1]);
            exit(1);
        }

        if (n2 == 1) {
            printf("Child PID: %d, I am the winner\n", getpid());
        }
        else if (n2 == -1) {
            printf("Child PID: %d, I am the loser\n", getpid());
        }
        else {
            printf("Child PID: %d, It's a tie\n", getpid());
        }

        close(c2_2p[1]);

        exit(0);
    }

    close(c1_2p[1]);
    close(c2_2p[1]);

    int n1, n2;
    if (read(c1_2p[0], &n1, sizeof(n1)) == -1) {
        perror("read c1");
        close(c1_2p[0]);
        close(c2_2p[0]);
        wait(0);
        wait(0);
        exit(1);
    }
    if (read(c2_2p[0], &n2, sizeof(n2)) == -1) {
        perror("read c2");
        close(c1_2p[0]);
        close(c2_2p[0]);
        wait(0);
        wait(0);
        exit(1);
    }

    printf("Parent PID: %d, Random number 1: %d, Random number 2: %d\n", getpid(), n1, n2);

    close(c1_2p[0]);
    close(c2_2p[0]);
    
    if (n1 > n2) {
        n1 = 1;
        n2 = -1;  
    }
    else if (n1 < n2) {
        n1 = -1;
        n2 = 1;
    }
    else {
        n1 = 0;
        n2 = 0;
    }



    close(p2c1[0]);
    close(p2c2[0]);

    if (write(p2c1[1], &n1, sizeof(n1)) == -1) {
        perror("write p2c1");
        close(p2c1[1]);
        close(p2c2[1]);
        wait(0);
        wait(0);
        exit(1);
    }
    if (write(p2c2[1], &n2, sizeof(n2)) == -1) {
        perror("write p2c2");
        close(p2c1[1]);
        close(p2c2[1]);
        wait(0);
        wait(0);
        exit(1);
    }

    wait(0);
    wait(0);
    return 0; 
}



