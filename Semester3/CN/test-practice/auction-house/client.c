#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <pthread.h>

#define PORT_UDP 1234
#define PORT_TCP 2222
// #define HOST "172.20.10.10"
#define HOST "192.168.0.104"

void error(const char *msg) {
    perror(msg);
    exit(1);
}

int current_price = 0;
pthread_mutex_t price_mutex = PTHREAD_MUTEX_INITIALIZER;

void *udp_listener(void *arg) {
    int udp_sockfd;
    struct sockaddr_in udp_serv_addr;
    char buffer[1024];
    socklen_t addr_len = sizeof(udp_serv_addr);

    // Create UDP socket
    if ((udp_sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        error("UDP socket creation error");
    }

    // Set SO_REUSEADDR
    int reuse = 1;
    if (setsockopt(udp_sockfd, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse)) == -1) {
        close(udp_sockfd);
        error("setsockopt SO_REUSEADDR failed");
    }

    // Bind the socket to the address and port
    memset(&udp_serv_addr, 0, sizeof(udp_serv_addr));
    udp_serv_addr.sin_family = AF_INET;
    udp_serv_addr.sin_port = htons(PORT_UDP);
    udp_serv_addr.sin_addr.s_addr = INADDR_ANY;

    if (bind(udp_sockfd, (struct sockaddr *)&udp_serv_addr, sizeof(udp_serv_addr)) < 0) {
        close(udp_sockfd);
        error("UDP bind failed");
    }

    printf("Listening for UDP broadcasts on port %d\n", PORT_UDP);

    while (1) {
        int n = recvfrom(udp_sockfd, buffer, sizeof(buffer), 0, (struct sockaddr *)&udp_serv_addr, &addr_len);
        if (n < 0) {
            close(udp_sockfd);
            error("UDP receive failed");
        }

        if (n == sizeof(int)) {
            int received_value;
            memcpy(&received_value, buffer, sizeof(int));
            received_value = ntohl(received_value);


            pthread_mutex_lock(&price_mutex);
            if (received_value > current_price) {
                current_price = received_value;
                printf("[Broadcast] new price is: %d\n", current_price);  
            }
            pthread_mutex_unlock(&price_mutex);

        } else {
            printf("Received unexpected number of bytes: %d\n", n);
        }
    }

    close(udp_sockfd);
    return NULL;
}

int raise_bid() {
    int bid;
    pthread_mutex_lock(&price_mutex);
    bid = current_price + 1;
    pthread_mutex_unlock(&price_mutex);

    sleep(1); // sleep for 1 second
    
    return bid;
}


int main() {
    int tcp_sockfd;
    struct sockaddr_in tcp_serv_addr;
    int bid;
    pthread_t udp_thread;

    // Create TCP socket
    if ((tcp_sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        error("TCP socket creation error");
    }

    // Set SO_REUSEADDR
    int reuse = 1;
    if (setsockopt(tcp_sockfd, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse)) == -1) {
        close(tcp_sockfd);
        error("setsockopt SO_REUSEADDR failed");
    }

    // Initialize TCP server address
    memset(&tcp_serv_addr, 0, sizeof(tcp_serv_addr));
    tcp_serv_addr.sin_family = AF_INET;
    tcp_serv_addr.sin_port = htons(PORT_TCP);
    if (inet_pton(AF_INET, HOST, &tcp_serv_addr.sin_addr) <= 0) {
        close(tcp_sockfd);
        error("Invalid address/ Address not supported");
    }

    // Connect to the TCP server
    if (connect(tcp_sockfd, (struct sockaddr *)&tcp_serv_addr, sizeof(tcp_serv_addr)) < 0) {
        close(tcp_sockfd);
        error("TCP connect failed");
    }

    // Start the UDP listener thread
    if (pthread_create(&udp_thread, NULL, udp_listener, NULL) != 0) {
        close(tcp_sockfd);
        error("Failed to create UDP listener thread");
    }

    // Receive welcome message from the server
    char buffer[1024];
    int valread = read(tcp_sockfd, buffer, sizeof(buffer) - 1);
    if (valread < 0) {
        close(tcp_sockfd);
        error("TCP read failed");
    }
    buffer[valread] = '\0';
    printf("%s\n", buffer);

    // Main loop to send bids
    while (1) {
        // Display the current price
        pthread_mutex_lock(&price_mutex);
        printf("Current price: %d\n", current_price);
        pthread_mutex_unlock(&price_mutex);

        // Get the user's bid
        printf("Enter your bid: ");
        pthread_mutex_lock(&price_mutex);
        bid = current_price + rand() % 10 + 1;
        // sleep for 1 second
        usleep(1000000);
        printf("You bid: %d\n", bid);
        pthread_mutex_unlock(&price_mutex);

        // Check if the bid is higher than the current price
        pthread_mutex_lock(&price_mutex);
        if (bid < current_price) {
            printf("Your bid must be higher than the current price\n");
            continue;
        }
        pthread_mutex_unlock(&price_mutex);

        // Send bid to the server
        int bid_network_order = htonl(bid);
        if (send(tcp_sockfd, &bid_network_order, sizeof(bid_network_order), 0) < 0) {
            close(tcp_sockfd);
            error("Send failed");
        }

        char answer;
        if (recv(tcp_sockfd, &answer, 1, 0) < 0) {
            close(tcp_sockfd);
            error("Receive failed");
        }

        if (answer == 's') {
            printf("Your bid is successful\n");
        } else if (answer == 'f') {
            printf("Your bid is failed\n");
        } 

        if (current_price >= 100) {
            printf("You are the winner\n");
            break;
        }

        // if (answer == 'w') {
        //     printf("You are the winner\n");
        //     break;
        // } else if (answer == 'l') {
        //     printf("You are the loser\n");
        //     break;
        // }
    }

    close(tcp_sockfd);
    pthread_cancel(udp_thread);
    pthread_join(udp_thread, NULL);

    return 0;
}
