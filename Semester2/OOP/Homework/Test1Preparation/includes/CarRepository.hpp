#pragma once
#include "Car.hpp"
#include <string>
#include <vector>


class CarRepository {
private:
    std::vector<Car> cars;

    bool carExists(const Car &car) const;

public:
    CarRepository();

    void addCar(const Car &car);

    void removeCar(const Car &car);

    std::vector<Car> getCars() const;

};