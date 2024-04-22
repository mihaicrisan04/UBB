#pragma once
#include "HospitalDepartment.h"
#include <string>


class Surgery : public HospitalDepartment {
    private:
        int numPatients;
    
    public:
        Surgery(std::string name, int numDoctors, int numPatients) : HospitalDepartment(name, numDoctors), numPatients(numPatients) {}

        bool isEfficient() override {
            return numPatients / numDoctors >= 2;
        }

        std::string toString() override {
            return "Surgery: " + name + " " + std::to_string(numDoctors) + " " + std::to_string(numPatients);
        }

        ~Surgery() override = default;
};