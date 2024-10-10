package controller;

import model.Vehicle;
import repository.Repository;

public class Controller {
    private Repository repo;

    public Controller(Repository repo) {
        this.repo = repo;
    }

    public void addVehicle(Vehicle vehicle) {
        repo.add(vehicle);
    }

    public void removeVehicle(int index) {
        repo.remove(index);
    }

    public Vehicle[] getAllVehicles() {
        return repo.getAll();
    }

    public Vehicle getVehicle(int index) {
        return repo.get(index);
    }

    public Vehicle[] getVehiclesWithRepairPriceOver(int price) {
        Vehicle[] vehicles = repo.getAll();
        Vehicle[] result = new Vehicle[repo.size()];
        int size = 0;
        for (Vehicle vehicle : vehicles) {
            if (vehicle.getRepairPrice() >= price) {
                result[size++] = vehicle;
            }
        }
        Vehicle[] finalResult = new Vehicle[size];
        for (int i = 0; i < size; i++) {
            finalResult[i] = result[i];
        }
        return finalResult;
    }
    
}
