#pragma once
#include "HospitalDepartment.h"
#include <string>


class NeonatalUnit : public HospitalDepartment {
    private:
        int numMothers;
        int numNewborns;
        double averageGrade;
    
    public:
        NeonatalUnit(std::string name, int numDoctors, int numMothers, int numNewborns, double averageGrade) : 
            HospitalDepartment(name, numDoctors),
            numMothers(numMothers),
            numNewborns(numNewborns),
            averageGrade(averageGrade) {}

        bool isEfficient() override {
            return averageGrade >= 8.5 && numNewborns >= numMothers;
        }

        std::string toString() override {
            return "NeonatalUnit: " + name + " " + std::to_string(numDoctors) + " " + std::to_string(numMothers) + " " + std::to_string(numNewborns) + " " + std::to_string(averageGrade); 
        }

        ~NeonatalUnit() override = default;
};