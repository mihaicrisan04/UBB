package model.statements;

import model.PrgState;
import model.exceptions.MyException;
import model.expressions.Exp;

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
        if (exp.eval(state.getSymTable(), state.getHeap()) != 0) {
            state.getExeStack().push(thenS);
        } else {
            state.getExeStack().push(elseS);
        }
        return null;
    }

    @Override
    public String toString() {
        return "IF(" + exp.toString() + ") THEN(" + thenS.toString() + ") ELSE(" + elseS.toString() + ")";
    }
    
}
