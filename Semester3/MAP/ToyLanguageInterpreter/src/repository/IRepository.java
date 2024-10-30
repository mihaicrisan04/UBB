package repository;

import java.io.IOException;

import model.PrgState;
import model.exceptions.MyException;
import collections.list.MyIList;

public interface IRepository {
    PrgState getCurrentProgram();
    MyIList<PrgState> getProgramList();
    void setProgramList(MyIList<PrgState> list);
    void logPrgStateExec() throws IOException;
}
