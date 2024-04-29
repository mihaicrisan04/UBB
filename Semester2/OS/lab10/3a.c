#include "3.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>


int main(int argc, char *argv[]) {  
    if (mkfifo(myfifo1, 0600) == -1) {
        if (errno != EEXIST) {
            perror("mkfifo");
            exit(1);
        }
    }
    if (mkfifo(myfifo2, 0600) == -1) {
        if (errno != EEXIST) {
            perror("mkfifo");
            exit(1);
        }
    }

    int a2b = open(myfifo1, O_WRONLY);
    if (a2b == -1) {
        perror("open");
        exit(1);
    }
    int b2a = open(myfifo2, O_RDONLY);
    if (b2a == -1) {
        perror("open");
        exit(1);
    }

    srand(time(NULL));
    
    char *s = malloc(sizeof(char) * 101); 
    printf("Enter a string: ");
    scanf("%s", s);
    s[100] = '\0';
    int len = strlen(s);
    while (len > 0) {
        printf("String before: %s\n", s);

        int n = rand() % len;
        len -= n;
        if (len < 0) {
            len = 0;
        }
        s[len] = '\0';

        printf("String after: %s\n", s);

        write(a2b, len, sizeof(int));
        if (len == 0) {
            break;
        }
        write(a2b, s, sizeof(char) * (len+1));

        read(b2a, &len, sizeof(int)); 
        if (len == 0) {
            break;
        }
        read(b2a, s, sizeof(char) * (len+1));
    }
    free(s);

    close(a2b);
    close(b2a);

    if (unlink(myfifo1) == -1) {
        perror("unlink");
        exit(1);
    }
    if (unlink(myfifo2) == -1) {
        perror("unlink");
        exit(1);
    }
    return 0;
}
