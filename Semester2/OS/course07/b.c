#include "header.h"


int main(int argc, char** argv) {
    int shmid;
    struct absp* x;

    shmid = shmget(1234, 0, 0);
    x = shmat(shmid, NULL, 0);

    srand(time(NULL));
    while (1) {
        x->s = x->a + x->b;
        x->p = x->a * x->b;
        if (x->p == x->s) {
            break;
        }
    }

    shmdt(x);
    shmctl(shmid, IPC_RMID, NULL);

    return 0;
}

