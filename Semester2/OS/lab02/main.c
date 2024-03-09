#include <stdio.h>
#include <stdlib.h>


int main(int argc, const char * argv[]) {
    if (argc < 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }
    FILE *f = fopen(argv[1], "r");
    if (f == NULL) {
        printf("Error: file not found\n");
        return 1;
    }
    char *s = malloc(sizeof(char) * 100);

    int k;

    FILE *out = fopen("out.txt", "w");
    if (out == NULL) {
        perror("Error opening file");
        return 1;
    }

//    fscanf(f, "%s", s);

    /*
    while (!feof(f)) {
        k = fread(s, 1, 100, f);
        if (k == 0) {
            perror("Error reading file");
        }
        s[k] = 0;
        fprintf(out, "%s", s);
    }
    */
    /*
    while ((k =fread(s, 1, 100, f)) > 0) {
        s[k] = 0;
        printf("%s", s);
    }
    */


    //printf("%s\n", s);

    free(s);
    fclose(f);
    fclose(out);
    return 0;
}
