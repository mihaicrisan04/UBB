#pragma once

#include <string>


class Department {
    public:
        std::string name;
        std::string description;

        Department(std::string name, std::string description);
        ~Department() = default;
};