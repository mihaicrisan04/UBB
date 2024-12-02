package model.statements;

import model.PrgState;
import model.exceptions.MyException;
import model.exceptions.StmtException;
import model.expressions.Exp;
import model.types.BoolType;
import model.values.BoolValue;
import model.values.Value;

public class IfStmt implements IStmt {
    private IStmt thenS;
    private IStmt elseS;
    private Exp exp;

    public IfStmt(Exp e, IStmt t, IStmt el) {
        exp = e;
        thenS = t;
        elseS = el;
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException, StmtException {
        Value val;
        try { val = exp.eval(prg.getSymTable(), prg.getHeap()); }
        catch (MyException e) { throw new StmtException("If statement: " + e.getMessage()); }

        if (!val.getType().equals(new BoolType())) { throw new StmtException("If statement: Expression is not a boolean"); }

        BoolValue b = (BoolValue) val;
        if (b.getValue()) { prg.getExeStack().push(thenS); } 
        else { prg.getExeStack().push(elseS); }

        return null;
    }

    @Override
    public IStmt deepCopy() {
        return new IfStmt(exp.deepCopy(), thenS.deepCopy(), elseS.deepCopy());
    }

    @Override
    public String toString() {
        return "IF(" + exp.toString() + ") THEN(" + thenS.toString() + ") ELSE(" + elseS.toString() + ")";
    }
    
}
