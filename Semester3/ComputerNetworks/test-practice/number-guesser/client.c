#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <time.h>

#define PORT_TCP 2222
#define HOST "172.20.10.10"

void error(const char *msg) {
    perror(msg);
    exit(1);
}

int main() {
    int sockfd;
    struct sockaddr_in serv_addr;
    char buffer[1024];
    int guess, sr = 0, er = (1 << 17) - 1, step_count = 0;
    char answer;
    int finished = 0;

    srand(time(NULL));

    // Create socket TCP
    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        error("Socket creation error");
    }

    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(PORT_TCP);

    // Convert IPv4 and IPv6 addresses from text to binary form
    if (inet_pton(AF_INET, HOST, &serv_addr.sin_addr) <= 0) {
        close(sockfd);
        error("Invalid address/ Address not supported");
    }

    // Connect to the server
    if (connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
        close(sockfd);
        error("Connection Failed");
    }

    // Receive welcome message
    int valread = read(sockfd, buffer, 1024);
    buffer[valread] = '\0';
    printf("%s\n", buffer);

    while (!finished) {
        guess = sr + rand() % (er - sr + 1);

        // Send guess
        int guess_network_order = htonl(guess);
        if (send(sockfd, &guess_network_order, sizeof(guess_network_order), 0) < 0) {
            error("Send failed");
        }

        // Receive answer
        if (recv(sockfd, &answer, 1, 0) < 0) { // Will wait until receive 1 byte
            error("Receive failed");
        }

        step_count++;
        printf("Sent %d Answer %c\n", guess, answer);

        if (answer == 'H') {
            sr = guess;
        } else if (answer == 'S') {
            er = guess;
        } else if (answer == 'G' || answer == 'L') {
            finished = 1;
        }

        usleep(250000); // Sleep for 0.25 seconds
    }

    close(sockfd);

    if (answer == 'G') {
        printf("I am the winner with %d in %d steps\n", guess, step_count);
    } else {
        printf("I lost !!!\n");
    }

    return 0;
}