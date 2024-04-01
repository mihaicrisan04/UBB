#include "Car.hpp"
#include <iostream>
#include <assert.h>

void testCar() {
    std::cout << "Running tests for Car class..." << std::endl;
    Car car1("Toyota", "Corolla", 2020, "Black");
    Car car2("Toyota", "Corolla", 2020, "Black");
    Car car3("Toyota", "Corolla", 2020, "White");
    Car car4("Toyota", "Corolla", 2021, "Black");
    Car car5("Toyota", "Camry", 2020, "Black");
    Car car6("Honda", "Civic", 2020, "Black");

    assert(car1 == car2);
    assert(!(car1 == car3));
    assert(!(car1 == car4));
    assert(!(car1 == car5));
    assert(!(car1 == car6));

    Car car7;
    car7 = car1;
    assert(car7 == car1);

    std::cout << "All tests passed!" << std::endl;
}