package repository;

import collections.list.MyIList;
import collections.list.MyList;
import model.PrgState;

public class Repository implements IRepository {
    MyIList<PrgState> programList;

    public Repository() {
        programList = new MyList<PrgState>();
    }
    
    public Repository(MyIList<PrgState> list) {
        programList = list;
    }

    @Override
    public PrgState getCurrentProgram() { return programList.get(0); }

    @Override
    public MyIList<PrgState> getProgramList() { return programList; }

    @Override
    public void setProgramList(MyIList<PrgState> list) { programList = list; }

    @Override
    public void logPrgStateExec() {
        System.out.println(getCurrentProgram().toString());
    }

    public void addProgram(PrgState prg) {
        programList.add(prg);
    }

    public void removeProgram(int index) {
        programList.remove(index);
    }

    public void removeProgram(PrgState prg) {
        for (int i = 0; i < programList.size(); i++) {
            if (programList.get(i).equals(prg)) {
                programList.remove(i);
                break;
            }
        }
    }

    public void clear() {
        programList.clear();
    }

    public int size() {
        return programList.size();
    }

    public PrgState getProgram(int index) {
        return programList.get(index);
    }

    public void clearCompleted() {
        for (int i = 0; i < programList.size(); i++) {
            if (!programList.get(i).isNotCompleted()) {
                programList.remove(i);
                i--;
            }
        }
    }
}
