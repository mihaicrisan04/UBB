package controller;

import repository.IRepository;

public class Controller {
    IRepository repo;

    public Controller() {
        repo = null;
    }

    public Controller(IRepository r) {
        repo = r;
    }

    public void oneStepForAllPrg() {
    }
}
