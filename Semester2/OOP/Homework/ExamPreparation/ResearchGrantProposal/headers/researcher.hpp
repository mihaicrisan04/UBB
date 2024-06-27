#pragma once

#include <string>

class Researcher {
    public:
        std::string name;
        std::string position;

        Researcher(std::string name, std::string position) : name(name), position(position) {}

        std::string toString() {
            return name + "," + position;
        }
};