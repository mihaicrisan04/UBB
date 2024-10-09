import repository.*;
import controller.*;
import view.*;

public class Main {
    public static void main(String[] args) {
        IRepository repo = new Repository();
        Controller ctrl = new Controller(repo);
        Console console = new Console(ctrl);
        console.run();
    }
}
