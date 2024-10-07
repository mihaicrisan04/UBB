import controller.Controller;
import model.Apple;
import model.Item;
import repository.CapacityExceededException;
import repository.IRepository;
import repository.Repository;
import view.Console;
//java.lang.System

public class ItemsApp {

    private void m(){}

    public static void main(String arr[]){

        Item a = new Apple(0.3f, "Gala");//hardcodat
        //preluare de parametri din linia de comanda array de intregi au stringuri
        //ItemsApp app =new ItemsApp();
        //app.m();

        IRepository repo = new Repository();
        try {
            repo.add(a);
        } catch (CapacityExceededException e) {
            System.out.println(e);
        }

        Controller ctrl = new Controller (repo);
        ctrl.add(a);

        Console console = new Console(ctrl);

        console.run();

        int i = 7;
        double d = i; // implicit
        int j = (int)d; //explicit in Java downcast explicit, upcast implicit
        System.out.println(i+d);
    }
}