package com.example.model.statements.semaphore;

import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Lock;
import java.util.List;

import javafx.util.Pair;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.exceptions.MyException;
import com.example.model.statements.IStmt;
import com.example.model.PrgState;
import com.example.model.types.IntType;
import com.example.model.types.Type;
import com.example.model.values.IntValue;


public class ReleaseStmt implements IStmt {
    private String var;
    private static final Lock lock = new ReentrantLock();

    public ReleaseStmt(String var) {
        this.var = var;
    }

    @Override 
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        if (!typeEnv.containsKey(var)) { throw new MyException("Variable is not defined"); }

        var typ = typeEnv.get(var);
        if (!typ.equals(new IntType())) { throw new MyException("Variable is not an integer"); }

        return typeEnv;
    }   

    @Override
    public PrgState execute(PrgState prg) throws MyException {
        lock.lock();
        try {
            var symTbl = prg.getSymTable();
            var semTbl = prg.getSemaphoreTable();

            if (!symTbl.containsKey(var)) { throw new MyException("Variable is not defined"); }
            if (!symTbl.get(var).getType().equals(new IntType())) { throw new MyException("Variable is not an integer"); }

            IntValue idx = (IntValue) symTbl.get(var);
            int index = idx.getValue();
            
            if (!semTbl.containsKey(index)) { throw new MyException("Semaphore is not defined"); }

            Pair<Integer, List<Integer>> sem = semTbl.get(index);

            if (sem.getValue().contains(prg.getId())) {
                sem.getValue().remove((Integer) prg.getId());
            }

            semTbl.update(index, new Pair<>(sem.getKey(), sem.getValue()));

            return null;
        } finally {
            lock.unlock();
        }
    }

    @Override
    public IStmt deepCopy() {
        return new ReleaseStmt(var);
    }

    @Override
    public String toString() {
        return "release(" + var + ")";
    }
    
}
