#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>

#define BUFFER_SIZE 1024

void send_number(int fd, int number) {
    write(fd, &number, sizeof(int));
}

int receive_number(int fd) {
    int number;
    int res = read(fd, &number, sizeof(int));
    if (res == -1) {
        perror("read");
        exit(1);
    }
    else if (res == 0) {
        printf("EOF\n");
        exit(0);
    }
    return number;
}

int main() {
    int a2b[2], b2c[2], c2a[2];
    pipe(a2b);
    pipe(b2c);
    pipe(c2a);

    int a, b, c;

    a = fork();
    if (a == -1) {
        perror("fork");
        exit(1);
    }
    else if (a == 0) {
        close(a2b[0]);
        close(b2c[0]);
        close(b2c[1]);
        close(c2a[1]);
        close(c2a[0]);

        int n = 2;
        printf("Enter %d numbers:\n", n);
        fflush(stdout);  // Ensure the output is immediately flushed
        send_number(a2b[1], n);
        for (int i = 0; i < n; i++) {
            int number = i + 5;
            scanf("%d", &number);
            send_number(a2b[1], number);
            printf("Child A sent: %d\n", number);  // Add print for debugging
            fflush(stdout);  // Ensure the output is immediately flushed
        }

        close(a2b[1]);
        exit(0);
    }

    b = fork();
    if (b == -1) {
        perror("fork");
        exit(1);
    }
    else if (b == 0) {
        srand(time(NULL));
        close(a2b[1]);
        close(b2c[0]);
        close(c2a[0]);
        close(c2a[1]);

        int n = receive_number(a2b[0]);
        send_number(b2c[1], n);
        printf("Child B received n: %d\n", n);  // Add print for debugging
        fflush(stdout);  // Ensure the output is immediately flushed
        for (int i = 0; i < n; i++) {
            int number = receive_number(a2b[0]);
            number += rand() % 4 + 2; 
            send_number(b2c[1], number);
            printf("Child B sent: %d\n", number);  // Add print for debugging
            fflush(stdout);  // Ensure the output is immediately flushed
        }

        close(a2b[0]);
        close(b2c[1]);
        exit(0);
    }

    c = fork();
    if (c == -1) {
        perror("fork");
        exit(1);
    }
    else if (c == 0) {
        close(a2b[0]);
        close(a2b[1]);
        close(b2c[1]);
        close(c2a[0]);

        int n = receive_number(b2c[0]);
        int sum = 0;
        printf("Child C received n: %d\n", n);  // Add print for debugging
        fflush(stdout);  // Ensure the output is immediately flushed
        for (int i = 0; i < n; i++) {
            int number = receive_number(b2c[0]);
            sum += number;
            printf("Child C received: %d, current sum: %d\n", number, sum);  // Add print for debugging
            fflush(stdout);  // Ensure the output is immediately flushed
        }
        send_number(c2a[1], sum);
        printf("Child C sent sum: %d\n", sum);  // Add print for debugging
        fflush(stdout);  // Ensure the output is immediately flushed

        close(b2c[0]);
        close(c2a[1]);
        exit(0);
    }

    close(a2b[0]);
    close(a2b[1]);
    close(b2c[0]);
    close(b2c[1]);
    close(c2a[1]);

    int sum =  receive_number(c2a[0]);
    printf("Parent received sum: %d\n", sum);  // Add print for debugging
    fflush(stdout);  // Ensure the output is immediately flushed

    close(c2a[0]);

    wait(NULL);
    wait(NULL);
    wait(NULL);

    return 0;
}

