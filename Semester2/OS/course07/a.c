#include "header.h"


int main(int argc, char** argv) {
    int shmid;
    struct absp* x;

    // Create shared memory; key = 1234; IPC_CREAT = create if not exist; 0600 = read/write permission
    shmid = shmget(1234, sizeof(struct absp), 0600 | IPC_CREAT);
    x = shmat(shmid, NULL, 0);

    srand(time(NULL));
    while (1) {
        x->a = rand() % 100;
        x->b = rand() % 100;
        if (x->p == x->s) {
            break;
        }
    }

    shmdt(x);
    shmctl(shmid, IPC_RMID, NULL);

    return 0;
}

