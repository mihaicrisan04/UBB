package model.statements;

import collections.dictionary.MyIDictionary;
import model.PrgState;
import model.exceptions.MyException;
import model.expressions.Exp;
import model.values.Value;

public class AssignStmt implements IStmt {
    String id;
    Exp exp;

    public AssignStmt(String id, Exp exp) {
        this.id = id;
        this.exp = exp;
    }
    
    @Override
    public PrgState execute(PrgState state) throws MyException{
        MyIDictionary<String, Value> symTable = state.getSymTable();

        if (!symTable.containsKey(id)) { throw new MyException("Variable " + id + " is not defined"); }

        Value val = exp.eval(symTable);
        if (val.getType().equals(symTable.get(id).getType())) { symTable.put(id, val); } 
        else { throw new MyException("Type of expression does not match type of variable"); }

        return state;
    }

    @Override
    public IStmt deepCopy() {
        return new AssignStmt(id, exp.deepCopy());
    }

    @Override
    public String toString() {
        return id + "=" + exp.toString();
    }
}
