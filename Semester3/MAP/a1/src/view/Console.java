package view;

import java.util.Scanner;
import java.util.InputMismatchException;
import controller.Controller;
import controller.ControllerException;
import model.Truck;
import model.Car;
import model.Motorcycle;
import model.Vehicle;

public class Console {
    private Controller ctrl;
    private Scanner scanner;

    public Console(Controller ctrl) {
        this.ctrl = ctrl;
        scanner = new Scanner(System.in);
    }

    public void run() {
        while (true) {
            printMenu();
            int cmd = readInt("Enter command: ", "Invalid command");
            if (cmd == 0) {
                break;
            }
            switch (cmd) {
                case 1:
                    addVehicle();
                    break;
                case 2:
                    removeVehicle();
                    break;
                case 3:
                    printAllVehicles();
                    break;
                case 4:
                    printVehiclesWithRepairPriceOver();
                    break;
                default:
                    System.out.println("Invalid command");
            }
        }
        scanner.close();
    }

    private void printMenu() {
        System.out.println("1. Add vehicle");
        System.out.println("2. Remove vehicle");
        System.out.println("3. Print all vehicles");
        System.out.println("4. Print vehicles with repair price over a given value");
        System.out.println("0. Exit");
    }

    private int readInt(String prompt, String errorMessage) {
        System.out.print(prompt);
        try {
            return scanner.nextInt();
        } catch (InputMismatchException e) {
            System.out.println(errorMessage);
            scanner.next(); // Clear the invalid input
            return -1;
        }
    }

    private boolean readBoolean(String prompt, String errorMessage) {
        System.out.print(prompt);
        try {
            return scanner.nextBoolean();
        } catch (InputMismatchException e) {
            System.out.println(errorMessage);
            scanner.next(); // Clear the invalid input
            return false;
        }
    }

    private void addVehicle() {
        int type = readInt("Enter the vehicle type (1-car/2-motorcycle/3-truck): ", "Invalid data, please enter a number");
        if (type < 1 || type > 3) {
            System.out.println("Invalid vehicle type");
            return;
        }

        System.out.print("Enter name: ");
        String name = scanner.next();
        if (name.isEmpty()) {
            System.out.println("Invalid name");
            return;
        }

        int repairPrice = readInt("Enter repair price: ", "Invalid data, please enter a number");
        if (repairPrice < 0) {
            System.out.println("Invalid repair price");
            return;
        }

        switch (type) {
            case 1:
                int numberOfDoors = readInt("Enter number of doors: ", "Invalid data, the number of doors must be a number");
                if (numberOfDoors < 2 || numberOfDoors > 5) {
                    System.out.println("Invalid number of doors");
                    return;
                }
                addVehicle(new Car(name, repairPrice, numberOfDoors));
                break;
            case 2:
                boolean hasSidecar = readBoolean("Has a side car(true/false): ", "Invalid data, please enter true or false");
                addVehicle(new Motorcycle(name, repairPrice, hasSidecar));
                break;
            case 3:
                int maxWeight = readInt("Enter max weight: ", "Invalid data, the max weight must be a number");
                if (maxWeight < 0) {
                    if (maxWeight < -1) { // -1 is the exit command
                        System.out.println("Invalid max weight");
                    }
                    return;
                }
                addVehicle(new Truck(name, repairPrice, maxWeight));
                break;
        }
    }

    private void addVehicle(Vehicle vehicle) {
        try {
            ctrl.addVehicle(vehicle);
        } catch (ControllerException ce) {
            System.out.println(ce.getMessage());
        }
    }

    private void removeVehicle() {
        Vehicle[] vehicles = ctrl.getAllVehicles();
        for (int i = 0; i < vehicles.length; i++) {
            System.out.println(i + ". " + vehicles[i]);
        }

        int index = readInt("Enter the index of the vehicle you want to remove: ", "Invalid index");

        try { 
            ctrl.removeVehicle(index);
        } catch (ControllerException ce) {
            System.out.println(ce.getMessage());
        }
    }

    private void printAllVehicles() {
        Vehicle[] vehicles = ctrl.getAllVehicles();
        for (Vehicle vehicle : vehicles) {
            System.out.println(vehicle);
        }
    }

    private void printVehiclesWithRepairPriceOver() {
        int price = readInt("Enter the price: ", "Invalid data, price must be a number");
        if (price < 0) {
            if (price < -1) { // -1 is the exit command
                System.out.println("Invalid price");
            }
            return;
        }

        Vehicle[] vehicles = ctrl.getVehiclesWithRepairPriceOver(price);

        if (vehicles.length == 0) {
            System.out.println("No vehicles with repair price over " + price);
            return;
        }

        for (Vehicle vehicle : vehicles) {
            System.out.println(vehicle);
        }
    }
}
