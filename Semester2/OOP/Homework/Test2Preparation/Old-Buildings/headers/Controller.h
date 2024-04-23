#pragma once
#include "Block.h"
#include "House.h"
#include <vector>
#include <fstream>


class Controller {
    private:
        std::vector<Building *> buildings;

    public:
        void addBuilding(Building *building);

        std::vector<Building *> getAllBuildings() const;

        std::vector<Building *> getAllToBeRestored() const;

        std::vector<Building *> getAllToBeDemolished() const;

        void writeToFile(std::string filename, std::vector<Building *> buildings) const;
};