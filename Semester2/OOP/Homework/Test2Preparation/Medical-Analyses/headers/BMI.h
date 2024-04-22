#pragma once
#include "MedicalAnalysis.h"
#include <string>


class BMI : public MedicalAnalysis {
    private:
        double value;

    public:
        BMI(std::string date, double value) : MedicalAnalysis(date), value(value) {}

        bool isResultOK() override {
            return value >= 18.5 && value <= 25;
        }

        std::string toString() override {
            return "BMI: " + std::to_string(value) + " " + getDate();
        }

        ~BMI() override = default;
};