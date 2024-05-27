#include "service.hpp"


Service::Service(std::string filename) : repo(filename) {}

Vegetable Service::getVegetableBy(std::string name) {
    std::vector<Vegetable> vegetables = getVegetables();
    for (auto v: vegetables) {
        if (v.name == name) {
            return v;
        }
    }
    return Vegetable("","","");
}

std::vector<Vegetable> Service::getVegetables() {
    return repo.getVegetables();
}

std::vector<std::string> Service::getFamilies() {
    std::vector<Vegetable> vegetables = getVegetables();
    std::vector<std::string> families;

    for (auto v: vegetables) {
        bool alreadyIn = false;
        for (auto f: families) {
            if (f == v.family) {
                alreadyIn = true;
                break;
            }
        }
        if (!alreadyIn) families.push_back(v.family);
    }

    return families;
}