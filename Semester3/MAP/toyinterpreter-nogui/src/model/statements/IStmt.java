package model.statements;

import collections.dictionary.MyIDictionary;
import model.types.Type;
import model.PrgState;
import model.exceptions.MyException;

public interface IStmt {
    MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException;
    PrgState execute(PrgState prg) throws MyException;
    String toString();
    IStmt deepCopy();
}