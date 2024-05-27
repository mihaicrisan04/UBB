#include "repository.hpp"

#include <fstream>
#include <string.h>


Repository::Repository(std::string filename) {
    readData(filename);
}

void Repository::readData(std::string filename) {
    std::ifstream fin(filename);

    char buffer[100];
    while (fin.getline(buffer, 99)) {
        std::string family = strtok(buffer, ",");
        std::string name = strtok(NULL, ",");
        std::string parts = strtok(NULL, "");

        // parts is supposed to be a list of strings not just a large string
        // TODO change parts to a vector of strings

        addVegetable(Vegetable(family, name, parts));
    }

    fin.close();
}

void Repository::addVegetable(Vegetable v) {
    vegetables.push_back(v);
}

std::vector<Vegetable> Repository::getVegetables() {
    return vegetables;
}


// Add tests 
// ...