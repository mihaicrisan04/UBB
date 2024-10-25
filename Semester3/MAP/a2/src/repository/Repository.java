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
        System.out.println(programList.get(0).toString());
    }
}
