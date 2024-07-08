#pragma once

#include <string>
#include <vector>
#include <tuple>

class Item {
public:
    std::string name;
    std::string category;
    int price;
    std::vector<std::tuple<int, std::string, int>> offers;

    Item(std::string name, std::string category, int price, std::vector<std::tuple<int, std::string, int>> offers = {}) : name(name), category(category), price(price), offers(offers) {}
    Item(Item const &item) : name(item.name), category(item.category), price(item.price), offers(item.offers) {}
};


class User {
public:
    std::string name;
    int id;
    std::string type;

    User(std::string name, int id, std::string type) : name(name), id(id), type(type) {}
};
