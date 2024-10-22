package model.statements;

import model.PrgState;
import model.expressions.Exp;

public class PrintStmt implements IStmt {
    private Exp exp;

    PrintStmt(Exp exp) {
        this.exp = exp;
    }

    @Override
    public PrgState execute(PrgState state) {

        return state;
    }

    @Override
    public String toString() {
        return "print(" + exp.toString() + ")";
    }
}
