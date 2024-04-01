#include "../includes/Car.h"


Car::Car() : name(""), model(""), year(0), color("") {}

Car::Car(const std::string &name, const std::string &model, int year, const std::string &color) : name(name), model(model), year(year), color(color) {}

Car::Car(const Car &other) : name(other.getName()), model(other.getModel()), year(other.getYear()), color(other.getColor()) {}

std::string Car::getName() const {
    return name;
}

std::string Car::getModel() const {
    return model;
}

int Car::getYear() const {
    return year;
}

std::string Car::getColor() const {
    return color;
}

bool Car::operator==(const Car &other) const {
    return model == other.getModel() && year == other.getYear();
}

bool Car::operator!=(const Car &other) const {
    return !(*this == other);
}

Car &Car::operator=(const Car &other)
{
    if (this != &other) {
        name = other.getName();
        model = other.getModel();
        year = other.getYear();
        color = other.getColor();
    }
    return *this;
}

std::string Car::toString() const {
    return name + " " + model + " " + std::to_string(year) + " " + color; 
}