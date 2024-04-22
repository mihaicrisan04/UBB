#pragma once
#include "MedicalAnalysis.h" 
#include <string>


class BP : public MedicalAnalysis {
    private:
        int systolicValue;
        int diastolicValue;

    public:
        BP(std::string date, int systolicValue, int diastolicValue) : MedicalAnalysis(date), systolicValue(systolicValue), diastolicValue(diastolicValue) {}

        bool isResultOK() override {
            return 90 <= systolicValue && systolicValue < 120 && 60 <= diastolicValue && diastolicValue < 80;
        }

        std::string toString() override {
            return "BP: " + std::to_string(systolicValue) + " " + std::to_string(diastolicValue) + " " + getDate();
        }

        ~BP() override = default;
};