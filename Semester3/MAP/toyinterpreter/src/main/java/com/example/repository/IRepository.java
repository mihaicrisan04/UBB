package com.example.repository;

import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.collections.list.MyIList;

public interface IRepository {
    MyIList<PrgState> getProgramList();
    void setProgramList(MyIList<PrgState> list);
    void logPrgStateExec(PrgState prg) throws MyException;
}
