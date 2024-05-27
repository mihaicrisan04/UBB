#include "repository.hpp"

#include <fstream>
#include <string.h>

Repository::Repository(std::string filename) {
    readData(filename);
}

void Repository::readData(std::string filename) {
    std::ifstream fin (filename);

    char buffer[100];

    while (fin.getline(buffer, 99)) {
        std::string category = strtok(buffer, ",");
        std::string name = strtok(NULL, ",");
        int quantity = atoi(strtok(NULL, ","));

        addItem(Item(category, name, quantity));
    }


    fin.close();
}

void Repository::addItem(Item item) {
    items.push_back(item);
}

void Repository::deleteItem(Item item) {
    for (int i = 0; i < items.size(); i++) {
        if (items[i].name == item.name) {
            items.erase(items.begin() + i);
            break;
        } 
    }
}

std::vector<Item> Repository::getItems() {
    return items;
}