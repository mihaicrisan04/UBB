package model.statements.heap;

import model.statements.IStmt;
import model.PrgState;
import model.exceptions.MyException;
import model.expressions.Exp;
import model.values.Value;
import model.values.RefValue;


public class NewHeapStmt implements IStmt {
    private String varName;
    private Exp exp;

    public NewHeapStmt(String varName, Exp exp) { 
        this.varName = varName;
        this.exp = exp;
    }

    public String getVarName() { return varName; }

    @Override
    public PrgState execute(PrgState state) throws MyException{
        var symTable = state.getSymTable();
        var heapTable = state.getHeapTable();

        if (!symTable.containsKey(varName)) { throw new MyException("Variable " + varName + " is not defined"); }

        Value val = exp.eval(symTable, heapTable);
        Integer address = heapTable.getNextFreeAddress();
        RefValue refVal = new RefValue(address, val.getType());

        heapTable.put(address, val);
        symTable.put(varName, refVal);

        return null;
    }

    @Override
    public String toString() { return "new(" + varName + ", " + exp.toString() + ")"; }

    @Override
    public IStmt deepCopy() { return new NewHeapStmt(varName, exp.deepCopy()); }    
}
