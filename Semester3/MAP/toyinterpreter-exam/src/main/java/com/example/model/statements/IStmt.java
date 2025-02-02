package com.example.model.statements;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.types.Type;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;

public interface IStmt {
    MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException;
    PrgState execute(PrgState prg) throws MyException;
    String toString();
    IStmt deepCopy();
}