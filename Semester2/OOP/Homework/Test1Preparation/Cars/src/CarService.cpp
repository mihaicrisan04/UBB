#include "../includes/CarService.h"


CarService::CarService(CarRepository &repo) : repo{repo} {}

void CarService::addCar(const Car &car) {
    try {
        repo.addCar(car);
    } catch (std::runtime_error &e) {
        throw e;
    }
}

void CarService::removeCar(const Car &car) {
    try {
        repo.removeCar(car);
    } catch (std::runtime_error &e) {
        throw e;
    }
}

std::vector<Car> CarService::getCars() const {
    return repo.getCars();
}

Car CarService::getCarAtPos(int pos) const {
    try {
        return repo.getCarAtPos(pos);
    } catch (std::out_of_range &e) {
        throw e;
    }
}

int CarService::getPosOfCar(const Car &car) const {
    return repo.getPosOfCar(car);
}


