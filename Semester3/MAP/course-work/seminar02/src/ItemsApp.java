import model.Item;
import model.Apple;
import repository.IRepository;
import repository.Repository;
import repository.CapacityExceededException;
import controller.Controller;
import view.Console;
// java.lang.system by default

public class ItemsApp {
    private void m() {}

    public static void main(String[] args) {
        Item a = new Apple(0.3f, "Gala");
        IRepository repo = new Repository();
        try {
            repo.add(a);
        } catch (CapacityExceededException e) {
            System.out.println(e);
        }

        Controller ctrl = new Controller(repo);
        ctrl.add(a);

        Console console = new Console(ctrl);
        console.run();
    }
}
