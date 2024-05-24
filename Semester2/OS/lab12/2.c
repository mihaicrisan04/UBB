#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/types.h>


int *col_sum;
int **mat;
int rows, cols;

void *g(void *arg) {
    int index = *(int *)arg;
    for (int i = 0; i < rows; i++) {
        col_sum[index] += mat[i][index];
    }
    return NULL;
}

int main(int argc, char *argv[]) {
    FILE *f = fopen("2.txt", "r");
    fscanf(f, "%d %d", &rows, &cols);

    mat = (int **)malloc(rows * sizeof(int *));
    for (int i = 0; i < rows; i++) {
        mat[i] = (int *)malloc(cols * sizeof(int));
        for (int j = 0; j < cols; j++) {
            fscanf(f, "%d", &mat[i][j]);
        }
    }

    col_sum = (int *)malloc(cols * sizeof(int));
    memset(col_sum, 0, cols * sizeof(int));
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            col_sum[j] += mat[i][j];
        }
    }
    for (int i = 0; i < cols; i++) {
        printf("%d ", col_sum[i]);
    }

    memset(col_sum, 0, cols * sizeof(int));
    pthread_t th[cols];
    int indexes[cols];
    for (int i = 0; i < cols; i++) {
        indexes[i] = i;
        pthread_create(&th[i], NULL, g, &indexes[i]);
    }
    for (int i = 0; i < cols; i++) {
        pthread_join(th[i], NULL);
    }


    for (int i = 0; i < rows; i++) {
        free(mat[i]);
    }
    free(mat);
    free(col_sum);
    fclose(f);
    return 0;
}
