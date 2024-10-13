package controller;

import model.Vehicle;
import repository.Repository;

public class Controller {
    private Repository repo;

    public Controller(Repository repo) {
        this.repo = repo;
    }

    public void addVehicle(Vehicle vehicle) throws ControllerException {
        try {
            repo.add(vehicle);
        } catch (Exception e) {
            throw new ControllerException(e.getMessage());
        }
    }

    public void removeVehicle(int index) throws ControllerException {
        try {
            repo.remove(index);
        } catch (Exception e) {
            throw new ControllerException(e.getMessage());
        }
    }

    public Vehicle[] getAllVehicles() {
        return repo.getAll();
    }

    public Vehicle getVehicle(int index) throws ControllerException {
        try {
            return repo.get(index);
        } catch (Exception e) {
            throw new ControllerException(e.getMessage());
        }
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
