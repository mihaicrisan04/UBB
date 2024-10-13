
import repository.Repository;
import controller.Controller;
import view.Console;

public class App {
    public static void main(String[] args) throws Exception {
        Repository repo = new Repository();
        Controller controller = new Controller(repo);
        Console console = new Console(controller);
        console.run();
    }
}
