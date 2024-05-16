#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>


int main() {
    int pipes[10][2];
    for (int i = 0; i < 10; i++) {
        pipe(pipes[i]);
    }

    for (int i = 0; i < 10; i++) {
        int f = fork();
        if (f == 0) {
            //struct timespec start, end;
            //clock_gettime(CLOCK_MONOTONIC, &start);
            int start = time(NULL);
            sleep(1);
            int end = time(NULL);
            double time = end - start;
            //clock_gettime(CLOCK_MONOTONIC, &end);
            //double time = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;
            printf("Child %d time: %f\n", i, time);
            write(pipes[i][1], &time, sizeof(time));
            exit(0);
        }
    }

    double total = 0;
    for (int i = 0; i < 10; i++) {
        double time;
        read(pipes[i][0], &time, sizeof(time));
        total += time;
        close(pipes[i][0]);
        close(pipes[i][1]);
        wait(NULL);
    }

    printf("Total time: %f\n", total);

    return 0;
}
