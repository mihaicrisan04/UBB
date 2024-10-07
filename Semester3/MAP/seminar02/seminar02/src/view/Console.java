package view;

import controller.Controller;
import model.Apple;
import model.Item;
import java.util.Scanner;

public class Console {
    private Controller ctrl;

    public Console(Controller ctrl) {
        this.ctrl = ctrl;
    }

    private void printMenu() {
        System.out.println("1. Add");
        System.out.println("2. Show all");
        System.out.println("3. Size");
        System.out.println("4. Filter by weight");
        System.out.println("0. Exit");
    }

    public void run() {
        int option;
        do {
            printMenu();
            System.out.println(">>>");
            Scanner scanner = new Scanner(System.in);
            option = Integer.parseInt(scanner.nextLine());
            executeOption(option);
        } while (option != 0);
    }

    private void executeOption(int option) {
        switch (option) {
            case 0: break;
            case 1: ctrl.add(readItem()); break;
            case 2: print(ctrl.all()); break;
            case 3: System.out.println(ctrl.size()); break;
            case 4: print(ctrl.filter(readWeight())); break;
        }
    }

    private Item readItem() {
        Item item = null;
        System.out.println("Item type (1 - Apple, 2 - Cake, 3 - Book): ");
        Scanner scanner = new Scanner(System.in);
        int type = Integer.parseInt(scanner.nextLine());
        switch (type) {
            case 1:
                System.out.println("Apple breed: ");
                String breed = scanner.nextLine();
                System.out.println("Apple weight: ");
                float weight = Float.parseFloat(scanner.nextLine());
                item = new Apple(weight, breed);
                break;
            case 2, 3: break;
            default: System.out.println("Invalid type");
        }
        return item;
    }

    private void print(Item[] items) {
        for (Item item : items) {
            System.out.println(item);
        }
    }

    private float readWeight() {
        System.out.println("Weight: ");
        Scanner scanner = new Scanner(System.in);
        return Float.parseFloat(scanner.nextLine());
    }
}
