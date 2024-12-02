package model.statements;

import model.PrgState;

public class NoOpStmt implements IStmt {
    public NoOpStmt() {}

    @Override
    public String toString() {
        return "NoOp";
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
