#include "CarRepository.hpp"
#include <stdexcept>


CarRepository::CarRepository() : cars() {}

bool CarRepository::carExists(const Car &car) const {
    for (const Car &c : cars) {
        if (c == car) {
            return true;
        }
    }
    return false;
}

void CarRepository::addCar(const Car &car) {
    if (carExists(car)) {
        throw std::invalid_argument("Car already exists!");
    }
    cars.push_back(car);
}

void CarRepository::removeCar(const Car &car) {
   if (!carExists(car)) {
       throw std::invalid_argument("Car does not exist!");
   }

}