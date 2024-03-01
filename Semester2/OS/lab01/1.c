#include <stdlib.h>
#include <stdio.h>


int main(int argc, char *argv[] ) {
	printf("argc = %d\n", argc);
	for (int i = 0; i < argc; i++) {
		printf("argv[%i] = %s\n",i, argv[i]);
	}
	printf("Hello world\n");
	int **x = malloc(sizeof(int*) * 10);
	for (int i = 0; i < 10; i++) {
		x[i] = malloc(sizeof(int) * 10);
	}
	for (int i = 0; i < 10; i++) {
		for (int j = 0; j < 10; j++) {
			x[i][j] = i * j;
		}
	}

    for (int i = 0; i < 10; i++) {
        for (int j = 0; j < 10; j++) {
            printf("%4d",  x[i][j]);
        }
        printf("\n");
    }
	
	for (int i = 0; i < 10; i++) {
		free(x[i]);
	}
    free(x);

	return 0;
}
