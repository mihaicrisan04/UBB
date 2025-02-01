package com.example.model.statements.countDownLatch;

import com.example.model.statements.IStmt;
import com.example.model.PrgState;
import com.example.collections.dictionary.MyIDictionary;
import com.example.model.exceptions.MyException;
import com.example.model.exceptions.StmtException;
import com.example.model.types.Type;
import com.example.model.types.IntType;
import com.example.model.values.IntValue;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;


public class AwaitStmt implements IStmt {
    private final String var;
    private static final Lock lock = new ReentrantLock();

    public AwaitStmt(String var) {
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
            var latchTbl = prg.getLatchTable();

            if (!symTbl.containsKey(var)) { throw new StmtException("Variable " + var + " is not defined"); }
            if (!symTbl.get(var).getType().equals(new IntType())) { throw new StmtException("Variable " + var + " is not an integer"); }

            IntValue idx = (IntValue) symTbl.get(var);
            int fi = idx.getValue();

            if (!latchTbl.containsKey(fi)) { throw new StmtException("Latch " + fi + " is not defined"); }

            if (latchTbl.get(fi) > 0) {
                prg.getExeStack().push(this);
            }

            return null;
        } finally {
            lock.unlock();
        }
    }

    @Override
    public IStmt deepCopy() {
        return new AwaitStmt(var);
    }

    @Override
    public String toString() {
        return "await(" + var + ")";
    }

}
