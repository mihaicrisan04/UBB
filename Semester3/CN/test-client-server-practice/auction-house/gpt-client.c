#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <pthread.h>

#define TCP_PORT 65432
#define UDP_PORT 65433
#define BUFFER_SIZE 1024

int highest_bid = 0;  // Latest bid from the server

void *udp_listener(void *arg) {
    int udp_socket;
    struct sockaddr_in udp_address;
    udp_socket = socket(AF_INET, SOCK_DGRAM, 0);

    udp_address.sin_family = AF_INET;
    udp_address.sin_addr.s_addr = htonl(INADDR_ANY);
    udp_address.sin_port = htons(UDP_PORT);

    bind(udp_socket, (struct sockaddr *)&udp_address, sizeof(udp_address));

    while (1) {
        int new_price;
        recvfrom(udp_socket, &new_price, sizeof(int), 0, NULL, NULL);
        new_price = ntohl(new_price);
        highest_bid = new_price;
        printf("New highest bid: %d\n", highest_bid);
    }
    close(udp_socket);
    return NULL;
}

int main() {
    int tcp_socket;
    struct sockaddr_in tcp_address;
    char buffer[BUFFER_SIZE];

    // Start UDP listener thread
    pthread_t udp_thread;
    pthread_create(&udp_thread, NULL, udp_listener, NULL);

    // Setup TCP socket
    tcp_socket = socket(AF_INET, SOCK_STREAM, 0);
    tcp_address.sin_family = AF_INET;
    tcp_address.sin_port = htons(TCP_PORT);
    inet_pton(AF_INET, "127.0.0.1", &tcp_address.sin_addr);  // Use local IP

    if (connect(tcp_socket, (struct sockaddr *)&tcp_address, sizeof(tcp_address)) < 0) {
        perror("TCP connection failed");
        return 1;
    }
    printf("Connected to auction house server.\n");

    // Main loop to submit bids
    while (1) {
        int bid;
        printf("Enter your bid (current highest: %d): ", highest_bid);
        scanf("%d", &bid);

        snprintf(buffer, BUFFER_SIZE, "%d", bid);
        send(tcp_socket, buffer, strlen(buffer), 0);

        // Receive response from server
        int bytes_received = recv(tcp_socket, buffer, BUFFER_SIZE, 0);
        if (bytes_received > 0) {
            buffer[bytes_received] = '\0';
            printf("Server: %s", buffer);
        }
    }

    close(tcp_socket);
    pthread_cancel(udp_thread);
    pthread_join(udp_thread, NULL);

    return 0;
}