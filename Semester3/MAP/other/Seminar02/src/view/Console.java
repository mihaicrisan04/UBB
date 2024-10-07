package view;

import controller.Controller;
import model.Apple;
import model.Item;

import java.util.Scanner;

public class Console{

    private Controller ctrl;

    public Console(Controller ctrl){
        this.ctrl=ctrl;
    }

    private void printMenu(){
        System.out.println("0-Exit");
        System.out.println("1-Add");
        System.out.println("2-All");
        System.out.println("3-Size");
        System.out.println("4-Filter");

    }

    public void run(){
        int option;
        do{
            printMenu();
            System.out.println("Option:");
            Scanner sc= new Scanner(System.in);
            option=Integer.parseInt(sc.nextLine());
            executeOption(option);
        }while (option!=0);
    }

    private void executeOption(int option){
        switch(option){
            case 0: break;
            case 1: ctrl.add(readItem()); break;
            case 2: print(ctrl.all()); break;
            case 3: System.out.println("Array size: "+ctrl.size()+" items." ); break;
            case 4: System.out.println("Filtered collection:\n");
                    print(ctrl.filter(readCriterion())); break;
        }
    }

    private Item readItem(){
        Item item=null;
        System.out.println("Provide the item type (1=Apple, 2=Cake, 3=Book):");
        Scanner sc= new Scanner(System.in);
        int type=Integer.parseInt(sc.nextLine());
        switch (type){
            case 1:
                System.out.println("Apple breed:");
                String breed =sc.nextLine();
                System.out.println("Apple weight:");
                float weight =Float.parseFloat(sc.nextLine());
                item=new Apple(weight, breed);
                break;
            case 2: break;
            case 3: break;
        }
        return item;
        }
    private void print(Item[] arr){
        for(Item f: arr){
            System.out.println(f);
        }
    }
    private float readCriterion(){
        System.out.println("Provide the filtering criterion:");
        Scanner sc= new Scanner(System.in);
        float weight=Float.parseFloat(sc.nextLine());

        return weight;

    }
}
