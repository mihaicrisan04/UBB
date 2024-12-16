#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>

void startClient(const char *host, int port) {
    int sock;
    struct sockaddr_in server_addr;
    char buffer[1024];
    char message[1024];

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        perror("Socket creation failed");
        exit(1);
    }

    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    if (inet_pton(AF_INET, host, &server_addr.sin_addr) <= 0) {
        perror("Invalid address/ Address not supported");
        close(sock);
        exit(1);
    }

    if (connect(sock, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0) {
        perror("Connection failed");
        close(sock);
        exit(1);
    }

    printf("Connected to the server. Type 'exit' to quit.\n");

    while (1) {
        printf("Enter a math problem to send (or 'exit' to quit): ");
        fgets(message, sizeof(message), stdin);
        message[strcspn(message, "\n")] = 0;  

        if (strcasecmp(message, "exit") == 0) {
            break;
        }

        send(sock, message, strlen(message), 0);

        int bytes_received = recv(sock, buffer, sizeof(buffer) - 1, 0);
        if (bytes_received < 0) {
            perror("Receive failed");
            break;
        }

        buffer[bytes_received] = '\0';
        printf("Server responded: %s\n", buffer);

        if (strcmp(buffer, "Invalid expression") == 0) {
            printf("Please enter a valid math problem.\n");
        }
    }

    close(sock);
    printf("Connection closed.\n");
}

int main() {
    startClient("172.20.10.12", 5433);
    return 0;
}