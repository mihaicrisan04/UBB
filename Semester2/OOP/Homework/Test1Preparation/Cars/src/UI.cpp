#include "../includes/UI.h"
#include <iostream>


UI::UI(CarService &service) : service{service} {}

void UI::printMenu() const {
    std::cout << "1. Add car\n";
    std::cout << "2. Remove car\n";
    std::cout << "3. Print cars\n";
    std::cout << "4. Print vintage cars\n";
    std::cout << "0. Exit\n";
}

void UI::addCar() {
    std::string name, model, color;
    int year;

    std::cout << "Enter name: ";
    std::cin >> name;
    std::cout << "Enter model: ";
    std::cin >> model;
    std::cout << "Enter year: ";
    std::cin >> year;
    std::cout << "Enter color: ";
    std::cin >> color;

    Car car(name, model, year, color);

    try {
        service.addCar(car);
    } catch (std::runtime_error &e) {
        std::cout << e.what() << std::endl;
    }
}

void UI::removeCar() {
    std::string name, model;
    int year;

    std::cout << "Enter name: ";
    std::cin >> name;
    std::cout << "Enter model: ";
    std::cin >> model;
    std::cout << "Enter year: ";
    std::cin >> year;

    Car car(name, model, year, "");

    try {
        service.removeCar(car);
    } catch (std::runtime_error &e) {
        std::cout << e.what() << std::endl;
    }
}

void UI::printCars() const {
    std::vector<Car> cars = service.getCars();
    std::sort(cars.begin(), cars.end(), [](const Car &a, const Car &b) {
        if (a.getName() == b.getName()) {
            return a.getModel() < b.getModel();
        }
        return a.getName() < b.getName();
    });

    for (const Car &car : cars) {
        std::cout << car.toString() << std::endl;
    }
}

void UI::printVintageCars() const {
    std::vector<Car> cars = service.getCars();
    std::sort(cars.begin(), cars.end(), [](const Car &a, const Car &b) {
        return a.getColor() < b.getColor();
    });

    for (const Car &car : cars) {
        if (car.getYear() < 2000) {
            std::cout << car.toString() << std::endl;
        }
    }
}

void UI::run() {
    int option;
    bool running = true;

    while (running) {
        printMenu();
        std::cout << "Enter option: ";
        std::cin >> option;

        switch (option) {
            case 1:
                addCar();
                break;
            case 2:
                removeCar();
                break;
            case 3:
                printCars();
                break;
            case 4:
                printVintageCars();
                break;
            case 0:
                running = false;
                break;
            default:
                std::cout << "Invalid option\n";
        }
    }
}


