#pragma once
#include "Building.h"


class Block: public Building {
    private:
        int totalApartments;
        int occupiedApartments;

    public:
        Block(std::string address, int constYear, int totalApartments, int occupiedApartments) : 
            Building(address, constYear),
            totalApartments(totalApartments),
            occupiedApartments(occupiedApartments) {}

        bool mustBeRestored() override {
            return 2024 - constYear >= 40 && (double)(occupiedApartments / totalApartments) > 0.8;
        }

        bool canBeDemolished() override {
            return (double)(occupiedApartments / totalApartments) < 0.05;
        }

        std::string toString() override {
            return "Block: " + address + " " + std::to_string(constYear) + " " + std::to_string(totalApartments) + " " + std::to_string(occupiedApartments);
        }

        ~Block() override = default;
};