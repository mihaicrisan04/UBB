#pragma once

#include "repository.hpp"

class Service {
    private:
        Repository repository;

    public:
        Service(std::string filename);
        void addBill(Bill bill);
        void payBill(std::string serialNumber);
        std::vector<Bill> getBills();
        ~Service() = default;
};