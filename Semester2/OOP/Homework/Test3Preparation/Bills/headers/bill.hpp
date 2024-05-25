#pragma once

#include <iostream>
#include <string>

class Bill {
    public:
        std::string companyName;
        std::string serialNumber;
        double amount;
        bool paid;

        Bill(std::string companyName, std::string serialNumber, double amount, bool paid);

        void payBill();
        std::string toString();

        ~Bill();
};