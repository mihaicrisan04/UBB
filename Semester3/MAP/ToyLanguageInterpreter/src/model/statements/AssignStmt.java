package model.statements;

import collections.dictionary.MyIDictionary;
import collections.heap.MyIHeap;
import model.PrgState;
import model.exceptions.MyException;
import model.exceptions.StmtException;
import model.exceptions.VariableNotDefined;
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
    public PrgState execute(PrgState state) throws MyException, StmtException {
        MyIDictionary<String, Value> symTable = state.getSymTable();
        MyIHeap<Integer, Value> heapTable = state.getHeapTable();

        if (!symTable.containsKey(id)) { throw new VariableNotDefined("Variable " + id + " is not defined"); }

        Value val;
        try { val = exp.eval(symTable, heapTable); }
        catch (MyException e) { throw new StmtException(e.getMessage()); }

        if (val.getType().equals(symTable.get(id).getType())) { symTable.put(id, val); } 
        else { throw new StmtException("Type of expression does not match type of variable"); }

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
