package controller;

import repository.IRepository;
import model.exceptions.MyException;
import model.PrgState;
import collections.list.MyIList;

public class Controller {
    IRepository repo;

    public Controller() {
        repo = null;
    }

    public Controller(IRepository r) {
        repo = r;
    }

    public void oneStepForAllPrg(MyIList<PrgState> prgList) throws MyException {
        // TODO: implement this method
    }

    public void executeAllSteps() throws MyException {
        try {
            repo.logPrgStateExec();
            while (repo.getCurrentProgram().isNotCompleted()) {
                repo.getCurrentProgram().oneStep();
                repo.logPrgStateExec();
            }
        } catch (MyException e) {
            throw e;
        }
    }
}
