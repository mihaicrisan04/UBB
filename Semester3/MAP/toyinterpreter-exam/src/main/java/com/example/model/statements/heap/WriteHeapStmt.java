package com.example.model.statements.heap;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.statements.IStmt;
import com.example.model.values.RefValue;
import com.example.model.PrgState;
import com.example.model.types.Type;
import com.example.model.types.RefType;
import com.example.model.exceptions.MyException;
import com.example.model.expressions.Exp;

public class WriteHeapStmt implements IStmt {
    private String varName;
    private Exp exp;

    public WriteHeapStmt(String varName, Exp exp) {
        this.varName = varName;
        this.exp = exp;
    }

    public String getVarName() { return varName; }
    public Exp getExp() { return exp; }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        if (!typeEnv.containsKey(varName)) { throw new MyException("Write Heap Stmt: variable " + varName + " is not defined"); }

        Type varType = typeEnv.get(varName);
        Type expType = exp.typeCheck(typeEnv);

        if (!varType.equals(new RefType(expType))) { throw new MyException("Write Heap Stmt: right hand side and left hand side have different types"); }

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState state) throws MyException {
        var symTable = state.getSymTable();
        var heapTable = state.getHeap();

        // Check if the variable is defined
        if (!symTable.containsKey(varName)) { throw new MyException("Variable " + varName + " is not defined"); }

        var value = symTable.get(varName);

        // Check if the variable is a reference type
        if (!(value instanceof RefValue)) { throw new MyException("Variable " + varName + " is not a reference"); }
        RefValue refValue = (RefValue) value;

        int address = refValue.getAddress();

        // Check if the address from the ref value associated is defined in the heap
        if (!heapTable.containsKey(address)) { throw new MyException("Address " + value + " is not defined in the heap"); }

        var val = exp.eval(symTable, heapTable);

        heapTable.put(address, val);

        return null; 
    }

    @Override
    public String toString() { return "wH(" + varName + ", " + exp.toString() + ")"; }

    @Override
    public IStmt deepCopy() { return new WriteHeapStmt(varName, exp.deepCopy()); } 
}
