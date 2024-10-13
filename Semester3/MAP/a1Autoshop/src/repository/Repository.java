package repository;

import model.Vehicle;
import model.Car;
import model.Truck;
import model.Motorcycle;

public class Repository {
    private Vehicle[] vehicles;
    private int size;
    private int capacity;

    public Repository() {
        capacity = 10;
        size = 0;
        vehicles = new Vehicle[capacity];

        // add some default vehicles
        vehicles[size++] = new Car("Car1", 100, 4);
        vehicles[size++] = new Truck("Truck1", 200, 1000);
        vehicles[size++] = new Car("Car2", 150, 2);
        vehicles[size++] = new Truck("Truck2", 250, 2000);
        vehicles[size++] = new Motorcycle("Bike1", 50, false);
    }

    public void add(Vehicle vehicle) throws RepositoryException {
        if (size == capacity) {
            throw new RepositoryException("Repository is full");
        }
        vehicles[size++] = vehicle;
    }

    public Vehicle get(int index) throws RepositoryException {
        if (index < 0 || index >= size) {
            throw new RepositoryException("Invalid index");
        }
        return vehicles[index];
    }

    public int size() { return size; }

    public void remove(int index) throws RepositoryException {
        if (index < 0 || index >= size) {
            throw new RepositoryException("Invalid index");
        }
        for (int i = index; i < size - 1; i++) {
            vehicles[i] = vehicles[i + 1];
        }
        size--;
    }

    public Vehicle[] getAll() {
        // return a copy of the vehicles array with the actual references of the vehicles but
        // with a size equal to the number of vehicles so that the no null references are returned
        Vehicle[] result = new Vehicle[size];
        for (int i = 0; i < size; i++) {
            result[i] = vehicles[i];
        }
        return result;
    }
}
