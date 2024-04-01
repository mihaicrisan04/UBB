#include <iostream>
#include "includes/UI.h"
#include "includes/tests.h"

int main() {
    testAll();

    CarRepository repo;
    CarService service{repo};
    UI ui{service};

    ui.run();

    return 0;
}
