#include "Controller.h"


void Controller::addBuilding(Building *building) {
    buildings.push_back(building);
}

std::vector<Building *> Controller::getAllBuildings() const {
    return buildings;
}

std::vector<Building *> Controller::getAllToBeRestored() const {
    std::vector<Building *> result;
    for (auto &building: buildings) {
        if (building->mustBeRestored()) {
            result.push_back(building);
        }
    }
    return result;
}

std::vector<Building *> Controller::getAllToBeDemolished() const {
    std::vector<Building *> result;
    for (auto &building: buildings) {
        if (building->canBeDemolished()) {
            result.push_back(building);
        }
    }
    return result;
}

void Controller::writeToFile(std::string filename, std::vector<Building *> buildings) const {
    std::ofstream file(filename);
    for (auto &building: buildings) {
        file << building->toString() << '\n';
    }
    file.close();
}
