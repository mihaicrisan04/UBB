#include <iostream>
#include "UI.h"
#include "Controller.h"

int main() {
    Controller controller;
    UI ui(controller);

    ui.run();

    return 0;
}
