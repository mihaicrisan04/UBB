#pragma once
#include "Building.h"


class House: public Building {
    private:
        std::string type;
        bool isHistorical;

    public:
        House(std::string address, int constYear, std::string type, bool isHistorical) :
            Building(address, constYear),
            type(type),
            isHistorical(isHistorical) {}

        bool mustBeRestored() override {
            return 2024 - constYear >= 100;
        }

        bool canBeDemolished() override {
            return !isHistorical;
        }

        std::string toString() override {
            return "House: " + address + " " + std::to_string(constYear) + " " + type + " " + std::to_string(isHistorical);
        }

        ~House() override = default;
};
