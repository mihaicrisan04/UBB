#pragma once
#include "Controller.h"


class UI {
    private:
        Controller controller;

    public:
        UI(const Controller &controller) : controller(controller) {}

        void run();

        void addBuilding();

        void printAllBuildings();

        void printAllToBeRestored();

        void printAllToBeDemolished();

        void writeToFile();
};