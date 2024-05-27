#pragma once

#include "item.hpp"
#include <vector>

class Repository {
    private:
        std::vector<Item> items;

    public: 
        Repository(std::string filename);
        ~Repository() = default;

        void addItem(Item item);
        void deleteItem(Item item);
        std::vector<Item> getItems();

        void readData(std::string filename);
};