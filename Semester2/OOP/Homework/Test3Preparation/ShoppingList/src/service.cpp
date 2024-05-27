#include "service.hpp"


Service::Service(std::string filename) : repo(filename) {}

void Service::addItem(Item item) {
    repo.addItem(item);
}

void Service::deleteItem(Item item) {
    repo.deleteItem(item);
}

std::vector<Item> Service::getItems() {
    return repo.getItems();
}