#include "repository.hpp"

#include <fstream>
#include <string.h>

Repository::Repository(std::string filename) {
    readFromFile(filename);
}

void Repository::readFromFile(std::string filename) {
    std::ifstream file(filename);
    char buffer[100];
    while (file.getline(buffer, 100)) {
        std::string companyName = strtok(buffer, ";");
        std::string serialNumber = strtok(NULL, ";");
        double amount = atof(strtok(NULL, ";"));
        bool paid = (strstr(strtok(NULL, ";"), "true")) ? true : false;

        this->bills.push_back(Bill(companyName, serialNumber, amount, paid));
    }
    file.close();
}

void Repository::writeToFile(std::string filename) {
    std::ofstream file(filename);
    for (int i = 0; i < this->bills.size(); i++) {
        file << this->bills[i].companyName << ";" << this->bills[i].serialNumber << ";" << this->bills[i].amount << ";" << (this->bills[i].paid ? "true" : "false") << "\n";
    }
    file.close();
}

void Repository::addBill(Bill bill) {
    this->bills.push_back(bill);
}

void Repository::payBill(std::string serialNumber) {
    for (int i = 0; i < this->bills.size(); i++) {
        if (this->bills[i].serialNumber == serialNumber) {
            this->bills[i].payBill();
            return;
        }
    }
}

std::vector<Bill> Repository::getBills() {
    return this->bills;
}
