#pragma once
#include "Client.h"


class WealthyClient: public Client {
    private:
        double moneyFromInvestments;

    public:
        WealthyClient(std::string name, int salary, double moneyFromInvestments) : Client(name, salary), moneyFromInvestments(moneyFromInvestments) {}

        double totalIncome() override {
            return salary + moneyFromInvestments;
        }

        bool isInterested(Dwelling d) override  {
            return d.largeBankRate() < totalIncome() && d.isProfitable; 
        }

        std::string toString() override {
            return "Wealthy client " + name + " with total income " + std::to_string(totalIncome());
        }

        ~WealthyClient() override = default;
};