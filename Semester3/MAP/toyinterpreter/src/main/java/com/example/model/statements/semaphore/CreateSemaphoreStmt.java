package com.example.model.statements.semaphore;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.exceptions.MyException;
import com.example.model.types.IntType;
import com.example.model.types.Type;
import com.example.model.statements.IStmt;
import com.example.model.PrgState;
import com.example.model.exceptions.StmtException;
import com.example.model.expressions.Exp;
import com.example.model.values.IntValue;

import javafx.util.Pair;

import java.util.ArrayList;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;


public class CreateSemaphoreStmt implements IStmt {
    private final String var;
    private final Exp exp;
    private static final Lock lock = new ReentrantLock();

    public CreateSemaphoreStmt(String var, Exp exp) {
        this.var = var;
        this.exp = exp;
    }
    
    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        if (!typeEnv.containsKey(var)) { throw new MyException("Variable " + var + " is not defined"); }

        Type varType = typeEnv.get(var);
        Type expType = exp.typeCheck(typeEnv);

        if (!varType.equals(new IntType())) { throw new MyException("Variable " + var + " is not an integer"); }
        if (!expType.equals(new IntType())) { throw new MyException("Expression is not an integer"); }

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException, StmtException {
        lock.lock();

        var symTbl = prg.getSymTable();
        var heap = prg.getHeap();
        var semTbl = prg.getSemaphoreTable();

        IntValue val = (IntValue) exp.eval(symTbl, heap);
        int number = val.getValue();

        int freeAddress = semTbl.getFreeAddress();

        semTbl.put(freeAddress, new Pair<>(number, new ArrayList<>()));

        if (!symTbl.containsKey(var)) { throw new StmtException("Variable " + var + " is not defined"); }
        if (!symTbl.get(var).getType().equals(new IntType())) { throw new StmtException("Variable " + var + " is not an integer"); }

        symTbl.put(var, new IntValue(freeAddress));

        lock.unlock();
        return null;
    }

    @Override
    public IStmt deepCopy() {
        return new CreateSemaphoreStmt(this.var, this.exp); 
    }

    @Override
    public String toString() {
        return "CreateSemaphoreStmt()";
    }

}

