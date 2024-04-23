#include <iostream>
#include "UI.h" 
#include "RealEstateAgency.h"   


int main() {
    RealEstateAgency* agency = new RealEstateAgency();
    UI ui(agency);
    ui.run();
    return 0;
}
