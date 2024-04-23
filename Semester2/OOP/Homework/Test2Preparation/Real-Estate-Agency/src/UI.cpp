#include "UI.h"
#include <iostream>

UI::UI(RealEstateAgency* agency) : agency(agency) {}

UI::~UI() {
    delete agency;
}

void UI::printMenu() {
    std::cout << "1. Add client\n";
    std::cout << "2. Add dwelling\n";
    std::cout << "3. Remove client\n";
    std::cout << "4. Get interested clients\n";
    std::cout << "5. Write to file\n";
    std::cout << "0. Exit\n";
}

void UI::run() {
    agency->addClient(new NormalClient("John", 2000));
    agency->addClient(new WealthyClient("Alice", 5000, 10000));
    agency->addDwelling(Dwelling("apartment", 1000, true));
    agency->addDwelling(Dwelling("house", 2000, false));
    
    while (true) {
        printMenu();

        int command;
        std::cout << ">>>";
        std::cin >> command;

        if (command == 0) {
            break;
        }
        switch (command) {
        case 1:
            addClient();
            break;

        case 2:
            addDwelling();
            break;
        
        case 3:
            removeClient();
            break;

        case 4:
            getInterestedClients();
            break;

        case 5:
            writeToFile();
            break;
        
        default:
            std::cout << "Invalid command\n";
            break;
        }
    }
}

void UI::addClient() {
    std::string name;
    int salary;
    std::cout << "Name: ";
    std::cin >> name;
    std::cout << "Salary: ";
    std::cin >> salary;

    std::string type;
    std::cout << "Type (normal/wealthy): ";
    std::cin >> type;

    if (type == "normal") {
        agency->addClient(new NormalClient(name, salary));
    } else if (type == "wealthy") {
        double moneyFromInvestments;
        std::cout << "Money from investments: ";
        std::cin >> moneyFromInvestments;
        agency->addClient(new WealthyClient(name, salary, moneyFromInvestments)); 
    } else {
        std::cout << "Invalid type\n";
    }
}

void UI::addDwelling() {
    std::string type;
    double price;
    bool isProfitable;

    std::cout << "Type (apartment/house): ";
    std::cin >> type;
    std::cout << "Price: ";
    std::cin >> price;
    std::cout << "Is profitable (1/0): ";
    std::cin >> isProfitable;

    agency->addDwelling(Dwelling(type, price, isProfitable));
}

void UI::removeClient() {
    std::string name;
    std::cout << "Name: ";
    std::cin >> name;

    agency->removeClient(name);
}

void UI::getInterestedClients() {
    std::string type;
    double price;
    bool isProfitable;

    std::cout << "Type (apartment/house): ";
    std::cin >> type;
    std::cout << "Price: ";
    std::cin >> price;
    std::cout << "Is profitable (1/0): ";
    std::cin >> isProfitable;

    std::vector<Client*> interestedClients = agency->getInterestedClients(Dwelling(type, price, isProfitable));

    for (Client* c : interestedClients) {
        std::cout << c->toString() << "\n";
    }
}

void UI::writeToFile() {
    agency->writeToFile("../clients.txt");
}