#include <iostream>
#include "headers/UI.h"
#include "headers/tests.h"

int main() {
    testAll();

    PlayerRepository repo;
    PlayerService service(repo);
    UI ui(service);

    ui.run();

    return 0;
}
