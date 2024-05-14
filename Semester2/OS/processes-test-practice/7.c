#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/wait.h>


void send_random_number(int write_fd) {
    int number = rand() % 10 + 1;
    if (write(write_fd, &number, sizeof(number)) == -1) {
        perror("write");
        exit(EXIT_FAILURE);
    }
    printf("Sent number: %d\n", number);
}

int receive_number(int read_fd) {
    int number;
    int result = read(read_fd, &number, sizeof(number));
    if (result == -1) {
        perror("read");
        exit(EXIT_FAILURE);
    }
    else if (result == 0) {
        printf("End of file\n");
        exit(EXIT_SUCCESS);
    }
    printf("Received number: %d\n", number);
    return number;
}

int main() {
    int pipe1[2], pipe2[2];
    pid_t pid1, pid2;

    // Seed the random number generator
    srand(time(NULL));

    // Create the first pipe
    if (pipe(pipe1) == -1) {
        perror("pipe1");
        exit(EXIT_FAILURE);
    }

    // Create the second pipe
    if (pipe(pipe2) == -1) {
        perror("pipe2");
        exit(EXIT_FAILURE);
    }

    // Fork the first child
    if ((pid1 = fork()) == -1) {
        perror("fork1");
        exit(EXIT_FAILURE);
    }

    if (pid1 == 0) { // First child
        close(pipe1[0]);
        close(pipe2[1]);

        while (1) {
            send_random_number(pipe1[1]); // Send number to second child
            int number = receive_number(pipe2[0]); // Receive number from second child
            if (number == 10) {
                break;
            }
        }

        close(pipe1[1]);
        close(pipe2[0]);
        exit(EXIT_SUCCESS);
    }

    // Fork the second child
    if ((pid2 = fork()) == -1) {
        perror("fork2");
        exit(EXIT_FAILURE);
    }

    if (pid2 == 0) { // Second child
        close(pipe1[1]);
        close(pipe2[0]);

        while (1) {
            int number = receive_number(pipe1[0]); // Receive number from first child
            if (number == 10) {
                break;
            }
            send_random_number(pipe2[1]); // Send number to first child
        }

        close(pipe2[1]);
        close(pipe1[0]);
        exit(EXIT_SUCCESS);
    }

    // Parent process closes all pipe ends and waits for children
    close(pipe1[0]);
    close(pipe1[1]);
    close(pipe2[0]);
    close(pipe2[1]);

    wait(NULL);
    wait(NULL);

    return 0;
}

