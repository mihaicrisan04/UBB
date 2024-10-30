package view;

import collections.list.MyIList;
import controller.Controller;
import model.exceptions.MyException;
import model.PrgState;

public class Console {
    private Controller ctrl;

    public Console(Controller c) {
        ctrl = c;
    }   

    private void printMenu() {
        System.out.println("1. Input program");
        System.out.println("0. Exit");
    }

    private int getIntCommand() {
        int cmd = 0;
        try {
            cmd = Integer.parseInt(System.console().readLine());
        } catch (NumberFormatException e) {
            System.out.println("Invalid command");
        }
        return cmd;
    }

    private void InputProgram() {
        MyIList<PrgState> prgList = ctrl.getProgramList();
        for (int i = 0; i < prgList.size(); i++) {
            System.out.println("Program " + i + ":");
            if (prgList.get(i).isNotCompleted()) {
                System.out.println(prgList.get(i).getOriginalProgram().toString());
            }
            else {
                System.out.println("Program finished");
            }
        }

        System.out.println("Enter program:");
        int cmd = getIntCommand();
        if (cmd < 0 || cmd >= prgList.size()) {
            return;
        }

        if (!prgList.get(cmd).isNotCompleted()) {
            System.out.println("Program finished");
            return;
        }

        try {
            ctrl.executeAllSteps(cmd);
        } catch (MyException e) {
            System.out.println(e.getMessage());
        } 
    }

    public void run() {
        while (true) {
            printMenu();
            int cmd = getIntCommand();
            switch (cmd) {
                case 0:
                    return;

                case 1:
                    InputProgram();
                    break;
            
                default:
                    break;
            } 
        }
    }
}
