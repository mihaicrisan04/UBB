#pragma once
#include <iostream>
#include <string>


class HospitalDepartment {
    protected:
        std::string name;
        int numDoctors;

    public:
        HospitalDepartment(std::string name, int numDoctors) : name(name), numDoctors(numDoctors) {}

        bool operator<(const HospitalDepartment &other) const {
            return name < other.name;
        }
        
        virtual bool isEfficient() = 0;

        virtual std::string toString() = 0;

        virtual ~HospitalDepartment() = default;
};