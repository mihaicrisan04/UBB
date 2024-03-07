#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    int rows, cols, i, j;
    FILE* fd;
    int** m;

    if(argc <= 1) {
        fprintf(stderr, "No input file specified");
        exit(1);
    }

    fd = fopen(argv[1],"rb");
    if(fd == NULL) {
        perror("Failed to open input file");
        exit(1);
    }

    if(fread(&rows, sizeof(int), 1, fd) <= 0) {
        perror("Could not read the number of rows");
        exit(1);
    }

    if(fread(&cols, sizeof(int), 1, fd) <= 0) {
        perror("Could not read the number of columns");
        exit(1);
    }

    m = (int**)malloc(rows * sizeof(int*));
    for(i=0; i<rows; i++) {
        m[i] = (int*)malloc(cols * sizeof(int));
        for(j=0; j<cols; j++) {
            fread(&m[i][j], sizeof(int), 1, fd);
            printf("%2d" , m[i][j]);
        }
        printf("\n");
    }
    for (i = 0; i < rows; i++) {
        free(m[i]);
    }
    free(m);

    fclose(fd);
    return 0;
}
