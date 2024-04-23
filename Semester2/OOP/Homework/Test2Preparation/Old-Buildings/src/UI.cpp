#include "UI.h"
#include <iostream>


void UI::run() {
    controller.addBuilding(new Block("Str. Mihai Viteazu, nr. 1", 1990, 10, 5));
    controller.addBuilding(new House("Str. Mihai Viteazu, nr. 2", 1900, "wood", true));
    controller.addBuilding(new Block("Str. Mihai Viteazu, nr. 3", 2000, 20, 10));
    controller.addBuilding(new House("Str. Mihai Viteazu, nr. 4", 1950, "brick", false));
    controller.addBuilding(new Block("Str. Mihai Viteazu, nr. 5", 2010, 30, 15));
    controller.addBuilding(new House("Str. Mihai Viteazu, nr. 6", 1930, "brick", true));
    controller.addBuilding(new Block("Str. Mihai Viteazu, nr. 7", 2020, 40, 20));
    controller.addBuilding(new House("Str. Mihai Viteazu, nr. 8", 1910, "wood", false));

    while (true) {
        std::cout << "1. Add building\n";
        std::cout << "2. Print all buildings\n";
        std::cout << "3. Print all buildings that must be restored\n";
        std::cout << "4. Print all buildings that can be demolished\n";
        std::cout << "5. Write to file\n";
        std::cout << "0. Exit\n";
        int command;
        std::cin >> command;
        std::cin.ignore();
        switch (command) {
            case 1:
                addBuilding();
                break;
            case 2:
                printAllBuildings();
                break;
            case 3:
                printAllToBeRestored();
                break;
            case 4:
                printAllToBeDemolished();
                break;
            case 5:
                writeToFile();
                break;
            case 0:
                return;
            default:
                std::cout << "Invalid command\n";
        }
    }
}

void UI::addBuilding() {
    std::string address;
    int constYear;
    std::cout << "Address: ";
    std::getline(std::cin, address);
    std::cout << "Construction year: ";
    std::cin >> constYear;
    std::cin.ignore();
    std::string type;
    std::cout << "Type (block/house): ";
    std::getline(std::cin, type);
    if (type == "block") {
        int totalApartments;
        int occupiedApartments;
        std::cout << "Total apartments: ";
        std::cin >> totalApartments;
        std::cout << "Occupied apartments: ";
        std::cin >> occupiedApartments;
        controller.addBuilding(new Block(address, constYear, totalApartments, occupiedApartments));
    } else if (type == "house") {
        std::string type;
        bool isHistorical;
        std::cout << "Type: ";
        std::getline(std::cin, type);
        std::cout << "Is historical (0/1): ";
        std::cin >> isHistorical;
        controller.addBuilding(new House(address, constYear, type, isHistorical));
    } else {
        std::cout << "Invalid type\n";
    }
}

void UI::printAllBuildings() {
    for (const auto &building : controller.getAllBuildings()) {
        std::cout << building->toString() << '\n';
    }
}

void UI::printAllToBeRestored() {
    for (const auto &building : controller.getAllToBeRestored()) {
        std::cout << building->toString() << '\n';
    }
}

void UI::printAllToBeDemolished() {
    for (const auto &building : controller.getAllToBeDemolished()) {
        std::cout << building->toString() << '\n';
    }
}

void UI::writeToFile() {
    controller.writeToFile("../buildings_restored.txt", controller.getAllToBeRestored());
    controller.writeToFile("../buildings_demolished.txt", controller.getAllToBeDemolished());
    std::cout << "Files written\n";
}
