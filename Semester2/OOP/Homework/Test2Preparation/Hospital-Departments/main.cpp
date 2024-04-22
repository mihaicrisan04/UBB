#include <iostream>
#include "Controller.h"
#include "UI.h"


int main() {
    Controller controller;
    UI ui(controller);

    ui.run();

    return 0;
}
