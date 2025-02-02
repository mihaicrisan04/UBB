package com.example.model.statements;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.PrgState;
import com.example.model.exceptions.StmtException;
import com.example.model.exceptions.MyException;
import com.example.model.expressions.Exp;
import com.example.model.types.Type;
import com.example.model.values.Value;

public class PrintStmt implements IStmt {
    private Exp exp;

    public PrintStmt(Exp exp) {
        this.exp = exp;
    }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        exp.typeCheck(typeEnv);
        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException, StmtException {
        Value val;
        try { val = exp.eval(prg.getSymTable(), prg.getHeap()); } 
        catch (MyException e) { throw new StmtException("[Print statement]: " + e.getMessage()); }

        prg.getOut().add(val);

        return null;
    }

    @Override
    public IStmt deepCopy() {
        return new PrintStmt(exp.deepCopy());
    }

    @Override
    public String toString() {
        return "print(" + exp.toString() + ")";
    }
}
