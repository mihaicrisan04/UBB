package com.example.model.statements.lock;

import com.example.model.statements.IStmt;
import com.example.collections.dictionary.MyIDictionary;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.model.exceptions.StmtException;
import com.example.model.types.IntType;
import com.example.model.types.Type;
import com.example.model.values.IntValue;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;


public class LockStmt implements IStmt {
    private String var;
    private static final Lock lock = new ReentrantLock();


    public LockStmt(String var) {
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
            var lockTbl = prg.getLockTable();
    
            if (!symTbl.containsKey(var)) { throw new StmtException("Variable " + var + " is not defined"); }
            if (!symTbl.get(var).getType().equals(new IntType())) { throw new StmtException("Variable " + var + " is not an integer"); }
    
            IntValue idx = (IntValue) symTbl.get(var);
            int fi = idx.getValue();

            if (!lockTbl.containsKey(fi)) { throw new StmtException("Lock " + fi + " is not defined"); }

            if (lockTbl.get(fi) == -1) {
                lockTbl.update(fi, prg.getId());
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
        return new LockStmt(var);
    }

    @Override
    public String toString() {
        return "lock(" + var + ")";
    }
    
}
