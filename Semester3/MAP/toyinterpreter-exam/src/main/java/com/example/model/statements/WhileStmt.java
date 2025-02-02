package com.example.model.statements;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.exceptions.MyException;
import com.example.model.expressions.Exp;
import com.example.model.values.BoolValue;
import com.example.model.PrgState;
import com.example.model.types.Type;
import com.example.model.types.BoolType;
import com.example.model.values.Value;


public class WhileStmt implements IStmt {
    private Exp exp;
    private IStmt stmt;

    public WhileStmt(Exp exp, IStmt stmt) {
        this.exp = exp;
        this.stmt = stmt;
    }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        Type typexp = exp.typeCheck(typeEnv);
        if (!typexp.equals(new BoolType())) { throw new MyException("While statement: Expression is not a boolean"); }

        stmt.typeCheck(typeEnv.deepCopy());

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException {
        var symTbl = prg.getSymTable();
        var heap = prg.getHeap();

        Value val = exp.eval(symTbl, heap);
        if (!val.getType().equals(new BoolType())) { throw new MyException("Expression is not a boolean"); }

        BoolValue boolVal = (BoolValue) val;
        if (boolVal.getValue()) {
            prg.getExeStack().push(this);
            prg.getExeStack().push(stmt);
        }

        return null;
    }

    @Override 
    public String toString() {
        return "while(" + exp.toString() + ") {" + stmt.toString() + "}";
    }

    @Override
    public IStmt deepCopy() {
        return new WhileStmt(exp.deepCopy(), stmt.deepCopy());
    }
}
