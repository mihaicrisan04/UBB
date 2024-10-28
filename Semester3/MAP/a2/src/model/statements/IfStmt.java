package model.statements;

import model.PrgState;
import model.exceptions.MyException;
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
    public PrgState execute(PrgState state) throws MyException {
        try {
            Value val = exp.eval(state.getSymTable());
            
            if (!val.getType().equals(new BoolType())) { throw new MyException("If statement: Expression is not a boolean"); }

            BoolValue b = (BoolValue) val;
            if (b.getValue()) { state.getExeStack().push(thenS); } 
            else { state.getExeStack().push(elseS); }

        } catch (MyException e) {
            throw new MyException("If statement: " + e.getMessage());
        }
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
