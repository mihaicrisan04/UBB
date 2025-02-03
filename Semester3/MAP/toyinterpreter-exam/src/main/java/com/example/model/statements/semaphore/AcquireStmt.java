package com.example.model.statements.semaphore;

import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Lock;
import java.util.List;


import com.example.model.statements.IStmt;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.model.exceptions.StmtException;
import com.example.collections.dictionary.MyIDictionary;
import com.example.model.types.Type;
import com.example.model.values.IntValue;
import com.example.model.types.IntType;

import javafx.util.Pair;


public class AcquireStmt implements IStmt {
    private String var;
    private static final Lock lock = new ReentrantLock();

    public AcquireStmt(String var) {
        this.var = var;
    }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        if (!typeEnv.containsKey(var)) { throw new MyException("Variable " + var + " is not defined"); }

        Type varType = typeEnv.get(var);
        if (!varType.equals(new IntType())) { throw new MyException("Variable " + var + " is not an integer"); }

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException, StmtException {
        lock.lock();
        try {
            var symTbl = prg.getSymTable();
            var semTbl = prg.getSemaphoreTable();

            if (!symTbl.containsKey(var)) { throw new StmtException("Variable " + var + " is not defined"); }
            if (!symTbl.get(var).getType().equals(new IntType())) { throw new StmtException("Variable " + var + " is not an integer"); }

            IntValue idx = (IntValue) symTbl.get(var);
            int index = idx.getValue();

            if (!semTbl.containsKey(index)) { throw new StmtException("Semaphore " + index + " is not defined"); }

            Pair<Integer, List<Integer>> sem = semTbl.get(index);
            int nl = sem.getValue().size();
            int n1 = sem.getKey();

            if (n1 > nl) {
                if (!sem.getValue().contains(prg.getId())) {
                    sem.getValue().add(prg.getId());
                    semTbl.update(index, new Pair<>(n1, sem.getValue()));
                }
            } else {
                prg.getExeStack().push(this);
            }

            return null;
        } finally {
            lock.unlock();
        }
    }

    @Override
    public IStmt deepCopy() {
        return new AcquireStmt(var);
    }

    @Override
    public String toString() {
        return "acquire(" + var + ")";
    }
    
}
