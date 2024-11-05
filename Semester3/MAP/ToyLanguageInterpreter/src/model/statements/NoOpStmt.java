package model.statements;

import model.PrgState;

public class NoOpStmt implements IStmt {
    public NoOpStmt() {}

    @Override
    public String toString() {
        return "NoOp";
    }

    @Override
    public PrgState execute(PrgState state) {
        return state;
    }

    @Override
    public IStmt deepCopy() {
        return new NoOpStmt();
    }
}
