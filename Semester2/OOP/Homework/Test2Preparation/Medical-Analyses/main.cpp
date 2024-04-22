#include <iostream>
#include "UI.h"
#include "Person.h"


int main() {
    Person person("John");
    UI ui(person);

    ui.run();

    return 0;
}
