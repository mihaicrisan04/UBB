package repository;

import model.PrgState;
import model.exceptions.MyException;
import collections.list.MyIList;

public interface IRepository {
    MyIList<PrgState> getProgramList();
    void setProgramList(MyIList<PrgState> list);
    void logPrgStateExec(PrgState prg) throws MyException;
}
