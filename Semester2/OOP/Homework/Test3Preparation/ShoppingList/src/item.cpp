#include "item.hpp"


Item::Item(std::string category, std::string name, int quantity) : category(category), name(name), quantity(quantity) {}

Item::~Item() {}

std::string Item::toString() {
    return category + " " + name + " " + std::to_string(quantity);
}