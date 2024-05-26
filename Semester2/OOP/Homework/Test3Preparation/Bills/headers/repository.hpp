#pragma once

#include "bill.hpp"
#include <vector>


class Repository {
    private:
        std::vector<Bill> bills;

    public:
        Repository(std::string filename);
        void readFromFile(std::string filename);
        void addBill(Bill bill);
        void payBill(std::string serialNumber);
        std::vector<Bill> getBills();
        ~Repository() = default;
};