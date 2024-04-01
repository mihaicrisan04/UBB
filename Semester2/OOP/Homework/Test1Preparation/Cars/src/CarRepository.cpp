#include "../includes/CarRepository.h"


CarRepository::CarRepository() : cars() {
    // cars.push_back(Car("Fiat", "Bravo", 2007, "red"));
    // cars.push_back(Car("Fiat", "Idea", 2003, "black"));
    // cars.push_back(Car("Audi", "A5", 2007, "blue"));
    // cars.push_back(Car("BMW", "Coupe", 2013, "pink"));
    // cars.push_back(Car("Mercedes", "E-Class", 2010, "green"));
    // cars.push_back(Car("Ford", "Focus", 2005, "yellow"));
}

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
    cars.erase(std::remove(cars.begin(), cars.end(), car), cars.end());
}

std::vector<Car> CarRepository::getCars() const {
    return cars;
}

int CarRepository::getPosOfCar(const Car &car) const {
    for (int i = 0; i < cars.size(); i++) {
        if (cars[i] == car) {
            return i;
        }
    }
    return -1;
}

Car CarRepository::getCarAtPos(int pos) const {
    if (pos < 0 || pos >= cars.size()) {
        throw std::out_of_range("Invalid position!");
    }
    return cars[pos];
}