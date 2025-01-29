package com.example.model.statements.semaphore;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.exceptions.MyException;
import com.example.model.types.Type;
import com.example.model.statements.IStmt;
import com.example.model.PrgState;
import com.example.model.exceptions.StmtException;
import com.example.model.expressions.Exp;
import com.example.model.values.StringValue;

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

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException, StmtException {
        lock.lock();



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

