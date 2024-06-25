#pragma once

#include <string>

class Doctor {
    public:
        std::string name;
        std::string specialty;
        Doctor(std::string name, std::string specialty) : name(name), specialty(specialty) {}

        std::string toString() {
            return name + "|" + specialty;
        }
};