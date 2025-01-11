package com.example.model.statements;

import com.example.collections.dictionary.MyIDictionary;
import com.example.collections.heap.MyIHeap;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.model.exceptions.StmtException;
import com.example.model.exceptions.VariableNotDefined;
import com.example.model.expressions.Exp;
import com.example.model.types.Type;
import com.example.model.values.Value;

public class AssignStmt implements IStmt {
    String id;
    Exp exp;

    public AssignStmt(String id, Exp exp) {
        this.id = id;
        this.exp = exp;
    }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        if (!typeEnv.containsKey(id)) { throw new StmtException("Assignment: variable " + id + " is not defined"); }

        Type varType = typeEnv.get(id);
        Type expType = exp.typeCheck(typeEnv);

        if (!varType.equals(expType)) { throw new StmtException("Assignment: right hand side and left hand side have different types"); }

        return typeEnv;
    }
    
    @Override
    public PrgState execute(PrgState prg) throws MyException, StmtException {
        MyIDictionary<String, Value> symTable = prg.getSymTable();
        MyIHeap<Integer, Value> heapTable = prg.getHeap();

        if (!symTable.containsKey(id)) { throw new VariableNotDefined("Variable " + id + " is not defined"); }

        Value val;
        try { val = exp.eval(symTable, heapTable); }
        catch (MyException e) { throw new StmtException(e.getMessage()); }

        if (val.getType().equals(symTable.get(id).getType())) { symTable.put(id, val); } 
        else { throw new StmtException("Type of expression does not match type of variable"); }

        return null;
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
