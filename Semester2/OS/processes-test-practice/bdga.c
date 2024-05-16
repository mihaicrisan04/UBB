#include "bdgcommon.h"


int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <args..>\n", argv[0]);
        return 1;
    }
    
    if (mkfifo(argv[1], 0666) < 0) {
        perror("mkfifo");
        return 1;
    }

    int fd = open(argv[1], O_WRONLY);

    int p2a[2], a2p[2];
    int p2b[2], b2p[2];

    if (pipe(p2a) < 0 || pipe(a2p) < 0 || pipe(p2b) < 0 || pipe(b2p) < 0) {
        perror("pipe");
        return 1;
    }

    for (int i = 2; i < argc; i++) {
        int n = atoi(argv[i]);
        if (n % 2 == 0) {
            write(p2a[1], &n, sizeof(int));
        }
        else {
            write(p2b[1], &n, sizeof(int));
        }
    }
    close(p2a[1]);
    close(p2b[1]);

    int a = fork();
    if (a == -1) {
        perror("fork");
        exit(1);
    }
    else if (a == 0) {
        close(p2b[0]);
        close(b2p[1]);

        int sum = 0;
        int num;
        while (read(p2a[0], &num, sizeof(int)) > 0) {
            sum += num;
        }

        write(a2p[1], &sum, sizeof(int));

        close(p2a[0]);
        close(a2p[1]);
        exit(0);
    }

    int b = fork();
    if (b == -1) {
        perror("fork");
        exit(1);
    }
    else if (b == 0) {
        close(p2a[0]);
        close(a2p[1]);

        int product = 1;
        int num;
        while (read(p2b[0], &num, sizeof(int)) > 0) {
            product *= num;
        }

        write(b2p[1], &product, sizeof(int));

        close(p2b[0]);
        close(b2p[1]);
        exit(0);
    }

    close(p2a[0]);
    close(p2a[1]);

    close(a2p[1]);

    close(p2b[0]);
    close(p2b[1]);

    close(b2p[1]);

    int sum, product;
    read(a2p[0], &sum, sizeof(int));
    read(b2p[0], &product, sizeof(int));

    printf("Sum: %d\n", sum);
    printf("Product: %d\n", product);

    write(fd, &sum, sizeof(int));
    write(fd, &product, sizeof(int));

    close(a2p[0]);
    close(b2p[0]);
   
    wait(NULL); 
    wait(NULL); 

    close(fd);

    unlink(argv[1]);
    return 0; 
}
