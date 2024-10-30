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

    public int getNumberOfPrograms() {
        return repo.getProgramList().size();
    }

    public MyIList<PrgState> getProgramList() {
        return repo.getProgramList();
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
        } catch (Exception e) {
            throw new MyException(e.getMessage());
        }
    }

    public void executeAllSteps(int index) throws MyException {
        PrgState prg = repo.getProgramList().get(index);
        try {
            System.out.println(prg.toString());
            while (prg.isNotCompleted()) {
                prg.oneStep();
                System.out.println(prg.toString());
            }
        } catch (MyException e) {
            throw e;
        } catch (Exception e) {
            throw new MyException(e.getMessage());
        }
    }
}
