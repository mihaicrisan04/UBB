#pragma once
#include "Controller.h"
#include <iostream>
#include <string>


class UI {
    private:
        Controller controller;

    public:
        UI(Controller controller);

        void printMenu() const;

        void AddNewDepartment();

        void ShowAllDepartments();

        void ShowAllEfficientDepartments();

        void writeToFile();

        void run();

        ~UI() = default;
};