#pragma once
#include <string>


class Dwelling {
    public:
        std::string type;
        double price;
        bool isProfitable;

        Dwelling(std::string type, double price, bool isProfitable) : type(type), price(price), isProfitable(isProfitable) {}

        double normalBankRate() {
            return price / 1000;
        }

        double largeBankRate() {
            return price / 100;
        }
};