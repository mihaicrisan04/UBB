#include "../includes/tests.h"

void testAll() {
    testCar();
    testCarRepository();
    testCarService();
    std::cout << "All tests passed!" << std::endl;
}

void testCar() {
    std::cout << "Running tests for Car class..." << std::endl;
    Car car1("Toyota", "Corolla", 2020, "Black");
    Car car2("Toyota", "Corolla", 2020, "Black");
    Car car3("Toyota", "Corolla", 2021, "White");
    Car car5("Toyota", "Camry", 2020, "Black");

    assert(car1.getName() == "Toyota");
    assert(car1.getModel() == "Corolla");
    assert(car1.getYear() == 2020);
    assert(car1.getColor() == "Black");

    assert(car1 == car2);
    assert(car1 != car3);
    assert(car1 != car5);

    Car car4 = car1;
    assert(car1 == car4);

    Car car;
    assert(car.getModel() == "");
    assert(car.getYear() == 0);

    car2 = car2;

    assert(car1.toString() == "Toyota Corolla 2020 Black");
}

void testCarRepository() {
    std::cout << "Running tests for CarRepository class..." << std::endl;
    CarRepository repo;

    Car car1("Toyota", "Corolla", 2020, "Black");
    Car car2("Toyota", "Corolla", 2021, "Black");
    Car car3("Toyota", "Corolla", 2021, "White");

    repo.addCar(car1);
    repo.addCar(car2);

    assert(repo.getPosOfCar(car1) == 0);
    assert(repo.getPosOfCar(car2) == 1);

    assert(repo.getCarAtPos(0) == car1);
    assert(repo.getCarAtPos(1) == car2);

    assert(repo.getCars().size() == 2);

    repo.removeCar(car1);
    assert(repo.getCars().size() == 1);

    try {
        repo.addCar(car3);
        assert(false);
    } catch (std::invalid_argument &e) {
        assert(true);
    }

    try {
        repo.removeCar(car3);
        assert(true);
    } catch (std::invalid_argument &e) {
        assert(true);
    }
    // no car should be left in the repo at this point

    try {
        repo.getCarAtPos(1);
        assert(false);
    } catch (std::out_of_range &e) {
        assert(true);
    }
}

void testCarService() {
    std::cout << "Running tests for CarService class..." << std::endl;
    CarRepository repo;
    CarService service(repo);

    Car car1("Toyota", "Corolla", 2020, "Black");
    Car car2("Toyota", "Corolla", 2021, "Black");
    Car car3("Toyota", "Corolla", 2021, "White");

    service.addCar(car1);
    service.addCar(car2);

    assert(service.getPosOfCar(car1) == 0);
    assert(service.getPosOfCar(car2) == 1);

    assert(service.getCarAtPos(0) == car1);
    assert(service.getCarAtPos(1) == car2);

    assert(service.getCars().size() == 2);

    service.removeCar(car1);
    assert(service.getCars().size() == 1);

    try {
        service.addCar(car3);
        assert(false);
    } catch (std::invalid_argument &e) {
        assert(true);
    }

    try {
        service.removeCar(car1);
        assert(false);
    } catch (std::invalid_argument &e) {
        assert(true);
    }

    try {
        service.getCarAtPos(1);
        assert(false);
    } catch (std::out_of_range &e) {
        assert(true);
    }
}