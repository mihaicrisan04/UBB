#include <iostream>
#include "headers/UI.h"


int main() {
    GeneRepository repo;
    GeneService service{repo};
    UI ui{service};

    ui.run();

    return 0;
}
