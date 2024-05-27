#pragma once

#include "repository.hpp"


class Service {
    private:
        Repository repo;

    public: 
        Service(std::string filename);
        ~Service() = default;

        Vegetable getVegetableBy(std::string name);
        std::vector<Vegetable> getVegetables();
        std::vector<std::string> getFamilies();
};