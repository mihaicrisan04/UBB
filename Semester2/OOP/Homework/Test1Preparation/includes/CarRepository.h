#pragma once
#include "Car.h"
#include <string>
#include <vector>
#include <algorithm>
#include <stdexcept>


class CarRepository {
private:
    std::vector<Car> cars;

    bool carExists(const Car &car) const;

public:
    CarRepository();

    void addCar(const Car &car);

    void removeCar(const Car &car);

    std::vector<Car> getCars() const;

    int getPosOfCar(const Car &car) const;

    Car getCarAtPos(int pos) const;
};