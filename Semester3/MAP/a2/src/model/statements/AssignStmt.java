package model.statements;

import model.PrgState;
import model.exceptions.MyException;
import model.expressions.Exp;

public class AssignStmt implements IStmt {
    String id;
    Exp exp;

    public AssignStmt(String id, Exp exp) {
        this.id = id;
        this.exp = exp;
    }
    
    @Override
    public PrgState execute(PrgState state) throws MyException{
        // TODO: implement this
        return state;
    }

    @Override
    public String toString() {
        return id + "=" + exp.toString();
    }
}
