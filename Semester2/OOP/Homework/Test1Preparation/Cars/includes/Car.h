#pragma once
#include <string>

class Car {
private:
    std::string name;
    std::string model;
    int year;
    std::string color;

public:
    Car();

    Car(const std::string &name, const std::string &model, int year, const std::string &color);

    Car(const Car &other);

    std::string getName() const;

    std::string getModel() const;

    int getYear() const;

    std::string getColor() const;

    bool operator==(const Car &other) const;

    bool operator!=(const Car &other) const;

    Car& operator=(const Car &other);

    std::string toString() const;

};