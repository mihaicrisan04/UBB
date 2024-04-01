#pragma once
#include "CarRepository.h"
#include <string>
#include <vector>
#include <algorithm>
#include <stdexcept>


class CarService {
private:
    CarRepository &repo;

public:
    CarService(CarRepository &repo);

    void addCar(const Car &car);

    void removeCar(const Car &car);

    std::vector<Car> getCars() const;

    Car getCarAtPos(int pos) const;

    int getPosOfCar(const Car &car) const;
};