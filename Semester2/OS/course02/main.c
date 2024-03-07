#include <stdio.h>
#include <string.h>
#include <unistd.h>

void with_lib() {
    char s[64];
    char* p;
    int i;
    FILE* x = stdout;
    
    while (1) {
        p = fgets(s, 64, stdin);
        if (p == NULL) {
            break;
        }
        for (i = 0; i < strlen(s); i++) {
            if (s[i] != '\n') {
                s[i] = 'l';
            }
        }

        if (x == stdout) {
            x = stderr;
        }
        else {
            x = stdout;
        }
        fputs(s, stdout);
    }
}


void with_syscall() {
    char s[64];
    int i, k, x = 1;

    while (1) {
        k = read(0, s, 64);
        if (k <= 0) {
            break;
        } 
        for (i = 0; i < k; i++) {
            if (s[i] != '\n') {
                s[i] = 's';
            }
        }
        write(1, s, k);
    }
}

int main(int argc, char** argv) {
    if (argc > 1 && strcmp(argv[1], "lib") == 0) {
        with_lib();
    }
    if (argc > 1 && strcmp(argv[1], "sys") == 0) {
        with_syscall();
    }
    return 0;
}

