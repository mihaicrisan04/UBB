package com.example.model.statements;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.model.exceptions.StmtException;
import com.example.model.expressions.Exp;
import com.example.model.types.Type;
import com.example.model.types.BoolType;
import com.example.model.values.BoolValue;
import com.example.model.values.Value;

public class IfStmt implements IStmt {
    private IStmt thenS;
    private IStmt elseS;
    private Exp exp;

    public IfStmt(Exp e, IStmt t, IStmt el) {
        exp = e;
        thenS = t;
        elseS = el;
    }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        Type typexp = exp.typeCheck(typeEnv);
        if (!typexp.equals(new BoolType())) { throw new MyException("If statement: Expression is not a boolean"); }

        thenS.typeCheck(typeEnv.deepCopy());
        elseS.typeCheck(typeEnv.deepCopy());

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException, StmtException {
        Value val;
        try { val = exp.eval(prg.getSymTable(), prg.getHeap()); }
        catch (MyException e) { throw new StmtException("If statement: " + e.getMessage()); }

        if (!val.getType().equals(new BoolType())) { throw new StmtException("If statement: Expression is not a boolean"); }

        BoolValue b = (BoolValue) val;
        if (b.getValue()) { prg.getExeStack().push(thenS); } 
        else { prg.getExeStack().push(elseS); }

        return null;
    }

    @Override
    public IStmt deepCopy() {
        return new IfStmt(exp.deepCopy(), thenS.deepCopy(), elseS.deepCopy());
    }

    @Override
    public String toString() {
        return "IF(" + exp.toString() + ") THEN(" + thenS.toString() + ") ELSE(" + elseS.toString() + ")";
    }
    
}
