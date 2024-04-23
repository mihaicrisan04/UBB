#pragma once
#include "Client.h"


class NormalClient: public Client {
    public:
        NormalClient(std::string name, int salary) : Client(name, salary) {}

        double totalIncome() override {
            return salary;
        }

        bool isInterested(Dwelling d) override {
            return d.normalBankRate() < totalIncome(); 
        }

        std::string toString() override {
            return "Normal client " + name + " with total income " + std::to_string(totalIncome());
        }

        ~NormalClient() override = default;
};
