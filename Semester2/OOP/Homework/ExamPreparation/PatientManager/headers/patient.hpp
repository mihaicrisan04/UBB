#pragma once

#include <string>


class Patient {
    public:
        std::string name;
        std::string diagnosis;
        std::string specialty;
        std::string doctor;
        std::string date;

        Patient(std::string name, std::string diagnosis, std::string specialty, std::string doctor, std::string date) :
         name(name), 
         diagnosis(diagnosis), 
         specialty(specialty), 
         doctor(doctor), 
         date(date) {
            if (diagnosis == "") {
                this->diagnosis = "undiagnosed";
            }
        }

        std::string toString() {
            return name + "|" + diagnosis + "|" + specialty + "|" + doctor + "|" + date;
        }
};