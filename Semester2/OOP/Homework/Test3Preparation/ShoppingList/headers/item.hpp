#pragma once 

#include <string>


class Item {
    public: 
        std::string category;
        std::string name;
        int quantity;

        Item(std::string category, std::string name, int quantity);
        ~Item();

        std::string toString();
};