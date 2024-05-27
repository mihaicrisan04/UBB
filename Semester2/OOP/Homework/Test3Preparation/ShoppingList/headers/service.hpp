#pragma once

#include "repository.hpp"


class Service {
    private:
        Repository repo;

    public:
        Service(std::string filename);
        ~Service() = default;

        void addItem(Item item);
        void deleteItem(Item item);
        std::vector<Item> getItems();
};