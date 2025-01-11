package com.example.model.statements.heap;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.statements.IStmt;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.model.expressions.Exp;
import com.example.model.types.Type;
import com.example.model.types.RefType;
import com.example.model.values.Value;
import com.example.model.values.RefValue;


public class NewHeapStmt implements IStmt {
    private String varName;
    private Exp exp;

    public NewHeapStmt(String varName, Exp exp) { 
        this.varName = varName;
        this.exp = exp;
    }

    public String getVarName() { return varName; }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        if (!typeEnv.containsKey(varName)) { throw new MyException("New statement: variable " + varName + " is not defined"); }

        Type varType = typeEnv.get(varName);
        Type expType = exp.typeCheck(typeEnv);

        if (!varType.equals(new RefType(expType))) { throw new MyException("New statement: right hand side and left hand side have different types"); }

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState state) throws MyException{
        var symTable = state.getSymTable();
        var heapTable = state.getHeap();

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
