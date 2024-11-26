package model.statements;

import model.PrgState;
import model.exceptions.StmtException;
import model.exceptions.MyException;
import model.expressions.Exp;
import model.values.Value;

public class PrintStmt implements IStmt {
    private Exp exp;

    public PrintStmt(Exp exp) {
        this.exp = exp;
    }

    @Override
    public PrgState execute(PrgState state) throws MyException, StmtException {
        Value val;
        try { val = exp.eval(state.getSymTable(), state.getHeapTable()); } 
        catch (MyException e) { throw new StmtException("[Print statement]: " + e.getMessage()); }

        state.getOut().add(val);

        return state;
    }

    @Override
    public IStmt deepCopy() {
        return new PrintStmt(exp.deepCopy());
    }

    @Override
    public String toString() {
        return "print(" + exp.toString() + ")";
    }
}
