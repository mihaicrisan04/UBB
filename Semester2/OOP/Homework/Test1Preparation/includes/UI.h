#pragma once
#include "CarService.h"


class UI {
private:
    CarService &service;

public:
    UI(CarService &service);

    void run();
    
    void printMenu() const;

    void addCar();

    void removeCar();

    // sorted by name and model
    void printCars() const;

    // cars older than 45 years, sorted by color
    void printVintageCars() const;
};