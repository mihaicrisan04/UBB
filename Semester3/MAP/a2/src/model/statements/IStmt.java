package model.statements;

import model.PrgState;
import model.exceptions.MyException;

public interface IStmt {
    PrgState execute(PrgState state) throws MyException;
}