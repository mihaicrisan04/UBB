package view;

import model.exceptions.MyException;
import controller.Controller;

public class Console {
    private Controller ctrl;

    public Console(Controller c) {
        ctrl = c;
    }   

    public void run() {
        try {
            ctrl.executeAllSteps();
        } catch (MyException e) {
            System.out.println(e.getMessage());
        } catch (Exception e) { 
            System.out.println(e.getMessage());
        }
    }
}
