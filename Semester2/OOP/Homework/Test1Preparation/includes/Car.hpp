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

    Car(std::string &name, std::string &model, int year, std::string &color);

    std::string getName() const;

    std::string getModel() const;

    int getYear() const;

    std::string getColor() const;

    bool operator==(const Car &other);

    Car& operator=(const Car &other);

    std::string toString() const;

};