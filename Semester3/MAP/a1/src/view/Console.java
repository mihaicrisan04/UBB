package view;

import model.Tree;
import model.Apple;
import controller.Controller;
import java.util.Scanner;

public class Console {
    private Controller ctrl;

    public Console(Controller ctrl) {
        this.ctrl = ctrl;
    }

    public void run() {
        ctrl.add(new Apple("Apple", 1, "Golden"));
        ctrl.add(new Apple("Apple", 2, "Red Delicious"));
        ctrl.add(new Apple("Apple", 3, "Granny Smith"));


        boolean running = true;
        while (running) {
            printMenu();
            int option = readOption();
            switch (option) {
                case 0:
                    running = false;
                    break;
                case 1:
                    addTree();
                    break;
                case 2:
                    removeTree();
                    break;
                case 3:
                    showAllTrees();
                    break;
                case 4:
                    filterTreesByAge();
                    break;
                default:
                    System.out.println("Invalid option");
            }
        }
    }

    private void printMenu() {
        System.out.println("1. Add tree");
        System.out.println("2. Remove tree");
        System.out.println("3. Show all trees");
        System.out.println("4. Filter trees by age");
        System.out.println("0. Exit");
    }

    private int readOption() {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Option: ");
        return scanner.nextInt();
    }

    private void addTree() {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Name: ");
        String name = scanner.nextLine();
        System.out.print("Age: ");
        int age = scanner.nextInt();
        System.out.print("Breed: ");
        String breed = scanner.nextLine();
        ctrl.add(new Apple(name, age, breed));
    }

    private void removeTree() {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Name: ");
        String name = scanner.nextLine();
        System.out.print("Age: ");
        int age = scanner.nextInt();
        ctrl.remove(new Apple(name, age, "Unknown"));
    }

    private void showAllTrees() {
        for (Tree tree: ctrl.all()) {
            System.out.println(tree);
        }
    }

    private void filterTreesByAge() {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Age: ");
        int age = scanner.nextInt();
        for (Tree tree: ctrl.filter(age)) {
            System.out.println(tree);
        }
    }
}
