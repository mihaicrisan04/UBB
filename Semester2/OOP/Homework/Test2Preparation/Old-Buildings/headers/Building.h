#pragma once
#include <string>


class Building {
    protected:
        std::string address;
        int constYear;

    public:
        Building(std::string address, int constYear) : address(address), constYear(constYear) {}

        virtual bool mustBeRestored() = 0;

        virtual bool canBeDemolished() = 0;

        virtual std::string toString() = 0;

        virtual ~Building() = default;
}; 