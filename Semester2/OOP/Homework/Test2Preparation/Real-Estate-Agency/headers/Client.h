#pragma once
#include <string>
#include "Dwelling.h"


class Client {
    protected:
        std::string name;
        int salary;

    public:     
        Client(std::string name, int salary) : name(name), salary(salary) {}

        std::string getName() const {
            return name;
        }

        virtual double totalIncome() = 0;

        virtual std::string toString() = 0;

        virtual bool isInterested(Dwelling d) = 0;

        virtual ~Client() = default;
};