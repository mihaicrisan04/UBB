package com.example.model.statements;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.model.types.Type;

public class NoOpStmt implements IStmt {
    public NoOpStmt() {}

    @Override
    public String toString() {
        return "NoOp";
    }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) {
        return null;
    }

    @Override
    public IStmt deepCopy() {
        return new NoOpStmt();
    }
}
