#pragma once

#include <string>
#include <vector>


class Volunteer {
    public:
        std::string name;
        std::string email;
        std::vector<std::string> interests; 
        std::string department;

        Volunteer(std::string name, std::string email, std::vector<std::string> interests, std::string department);
        ~Volunteer() = default;

        void setDepartment(std::string department);
};