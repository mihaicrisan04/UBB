package com.example.model.statements;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.model.types.Type;

public class ForkStmt implements IStmt {
    private IStmt stmt;

    public ForkStmt(IStmt stmt) {
        this.stmt = stmt;
    }

    public IStmt getStmt() { return stmt; }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        stmt.typeCheck(typeEnv.deepCopy());
        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) {
        return new PrgState(stmt, prg.getSymTable().deepCopy(), prg.getOut(), prg.getFileTable(), prg.getHeap(), prg.getSemaphoreTable(), prg.getLockTable(), prg.getLatchTable());
    }
    
    @Override
    public IStmt deepCopy() {
        return new ForkStmt(stmt.deepCopy());
    }

    @Override
    public String toString() {
        return "fork(" + stmt + ")";
    }
}
