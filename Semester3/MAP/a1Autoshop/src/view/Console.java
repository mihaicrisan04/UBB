package view;

import java.util.Scanner;
import controller.Controller;
import model.Truck;
import model.Car;
import model.Motorcycle;
import model.Vehicle;

public class Console {
    private Controller ctrl;

    public Console(Controller ctrl) {
        this.ctrl = ctrl;
    }

    public void run() {
        while (true) {
            printMenu();
            int cmd = readCommand();
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
            }
        }
    }
    
    private int readCommand() {
        System.out.print("Enter command: ");
        Scanner scanner = new Scanner(System.in);
        return scanner.nextInt();
    }

    private void printMenu() {
        System.out.println("1. Add vehicle");
        System.out.println("2. Remove vehicle");
        System.out.println("3. Print all vehicles");
        System.out.println("4. Print vehicles with repair price over a given value");
        System.out.println("0. Exit");
    }

    private void addVehicle() {
        // read vehicle data
        System.out.print("Enter the vehicle type (1-car/2-motorcycle/3-truck): ");
        Scanner scanner = new Scanner(System.in);
        int type = scanner.nextInt();

        System.out.print("Enter name: ");
        String name = scanner.next();

        System.out.print("Enter repair price: ");
        int repairPrice = scanner.nextInt();

        if (type == 1) {
            System.out.print("Enter number of doors: ");
            int numberOfDoors = scanner.nextInt();
            ctrl.addVehicle(new Car(name, repairPrice, numberOfDoors));
        } else if (type == 2) {
            System.out.print("Has a side car: (true/false): ");
            boolean hasSidecar = scanner.nextBoolean();
            ctrl.addVehicle(new Motorcycle(name, repairPrice, hasSidecar));
        } else if (type == 3) {
            System.out.print("Enter max weight: ");
            int maxWeight = scanner.nextInt();
            ctrl.addVehicle(new Truck(name, repairPrice, maxWeight));
        }
    }

    private void removeVehicle() {
        // read index
        Scanner scanner = new Scanner(System.in);
        int index = scanner.nextInt();

        // call controller.removeVehicle(index)
        ctrl.removeVehicle(index);
    }

    private void printAllVehicles() {
        Vehicle[] vehicles = ctrl.getAllVehicles();
        for (Vehicle vehicle : vehicles) {
            System.out.println(vehicle);
        }
    }

    private void printVehiclesWithRepairPriceOver() {
        // read price
        Scanner scanner = new Scanner(System.in);
        int price = scanner.nextInt();

        // call controller.getVehiclesWithRepairPriceOver(price)
        Vehicle[] vehicles = ctrl.getVehiclesWithRepairPriceOver(price);

        // print vehicles
        for (Vehicle vehicle : vehicles) {
            System.out.println(vehicle);
        }
    }
}
