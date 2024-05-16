#include "bdgcommon.h"

int gcd_euclid(int a, int b) {
    if (a == 0) return b;
    return gcd_euclid(b % a, a);
}


int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <fifo>\n", argv[0]);
        return 1;
    }

    int fd = open(argv[1], O_RDONLY);

    if (fd < 0) {
        perror("open");
        return 1;
    }

    int sum, product;
    read(fd, &sum, sizeof(sum));
    read(fd, &product, sizeof(product));

    int gcd = gcd_euclid(sum, product);
    printf("GCD of %d and %d is %d\n", sum, product, gcd);

    close(fd);

    return 0;
}
