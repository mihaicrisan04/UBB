#pragma once

#include <string>


class Vegetable {
    public: 
        std::string family;
        std::string name;
        std::string parts;

        Vegetable(std::string family, std::string name, std::string parts);
        ~Vegetable() = default;
};