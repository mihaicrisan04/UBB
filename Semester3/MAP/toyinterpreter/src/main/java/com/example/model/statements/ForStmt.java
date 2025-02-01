package com.example.model.statements;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.exceptions.MyException;
import com.example.model.expressions.Exp;
import com.example.model.expressions.*;
import com.example.model.enums.*;
import com.example.model.PrgState;
import com.example.model.types.IntType;
import com.example.model.types.Type;


public class ForStmt implements IStmt {
    private String id;
    private Exp exp1;
    private Exp exp2;
    private Exp exp3;
    private IStmt stmt;

    public ForStmt(String id, Exp exp1, Exp exp2, Exp exp3, IStmt stmt) {
        this.id = id;
        this.exp1 = exp1;
        this.exp2 = exp2;
        this.exp3 = exp3;
        this.stmt = stmt;
    }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        typeEnv.put(id, new IntType());

        Type exp1Type = exp1.typeCheck(typeEnv);
        Type exp2Type = exp2.typeCheck(typeEnv);
        Type exp3Type = exp3.typeCheck(typeEnv);
        
        if (!exp1Type.equals(new IntType()) || !exp2Type.equals(new IntType()) || !exp3Type.equals(new IntType())) { throw new MyException("For statement: Expressions are not integers"); }
        
        stmt.typeCheck(typeEnv.deepCopy());
        
        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException {
        IStmt newStmt = new CompoundStmt(
            new CompoundStmt(
                new VarDeclStmt(id, new IntType()),
                new AssignStmt(id, exp1)
            ),
            new WhileStmt(
                new CompareExp(
                    new VarExp(id),
                    exp2,
                    CompareOperation.LESS),
                new CompoundStmt(
                    stmt,
                    new AssignStmt(id, exp3)
                )
            )
        );

        prg.getExeStack().push(newStmt);

        return null;
    }
    @Override
    public IStmt deepCopy() {
        return new ForStmt(id, exp1.deepCopy(), exp2.deepCopy(), exp3.deepCopy(), stmt.deepCopy());
    }

    @Override
    public String toString() {
        return "for(" + id + " = " + exp1.toString() + "; " + id + " < " + exp2.toString() + "; " + id + " = " + exp3.toString() + ") {" + stmt.toString() + "}";
    }
}
