#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <time.h>

void send_number(int fd, int n) {
    if (write(fd, &n, sizeof(int)) < 0) {
        perror("write");
        exit(1);
    }
    printf("sent %d\n", n);
}

int recieve_number(int fd) {
    int n;
    int res = read(fd, &n, sizeof(int));
    if (res == -1) {
        perror("read");
        exit(1);
    }
    else if (res == 0) {
        printf("EOF\n");
        exit(0);
    }
    printf("recieved: %d\n", n);
    return n;
}

int main(int argc, char *arv[]) {
    int a2b[2], b2a[2];
    int a, b;

    if (pipe(a2b) < 0) {
        perror("pipe");
        exit(1);
    }
    if(pipe(b2a) < 0) {
        perror("pipe");
        exit(1);
    }

    a = fork();
    if (a == -1) {
        perror("fork");
        exit(1);
    }
    else if (a == 0) {
        srand(time(NULL));
        close(a2b[0]);
        close(b2a[1]);

        int n = rand() % 150 + 50;
        n += (n % 2 == 0) ? 0 : 1;
        printf("n = %d\n", n);

        while (1) {
            send_number(a2b[1], n);
            n = recieve_number(b2a[0]);
            if (n < 5) {
                break;
            }
        }

        close(a2b[1]);
        close(b2a[0]);
        exit(0);
    }

    b = fork();
    if (b == -1) {
        perror("fork");
        exit(1);
    }
    else if (b == 0) {
        close(a2b[1]);
        close(b2a[0]);

        int n;
        while (1) {
            n = recieve_number(a2b[0]);
            n /= 2;
            send_number(b2a[1], n);
        }

        close(a2b[0]);
        close(b2a[1]);
        exit(0);
    }
    close(a2b[0]);
    close(a2b[1]);
    close(b2a[0]);
    close(b2a[1]);

    wait(NULL);
    wait(NULL);
    return 0;
}
