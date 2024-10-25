package model.statements;

import model.PrgState;
import model.exceptions.MyException;
import model.expressions.Exp;
import model.values.Value;

public class PrintStmt implements IStmt {
    private Exp exp;

    public PrintStmt(Exp exp) {
        this.exp = exp;
    }

    @Override
    public PrgState execute(PrgState state) throws MyException {
        try {
            Value val = exp.eval(state.getSymTable());
            state.getOut().add(val);
            return state;
        } catch (MyException e) {
            throw e;
        }
    }

    @Override
    public String toString() {
        return "print(" + exp.toString() + ")";
    }
}
