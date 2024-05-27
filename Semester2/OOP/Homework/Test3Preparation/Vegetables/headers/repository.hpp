#pragma once

#include "vegetable.hpp"
#include <vector>


class Repository {
    private:
        std::vector<Vegetable> vegetables;
    
    public:
        Repository(std::string filename);   
        ~Repository() = default;

        void addVegetable(Vegetable v);
        std::vector<Vegetable> getVegetables();
        void readData(std::string filename);
};