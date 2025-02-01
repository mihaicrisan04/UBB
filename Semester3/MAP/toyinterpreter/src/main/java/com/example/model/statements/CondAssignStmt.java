package com.example.model.statements;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.types.Type;
import com.example.model.types.BoolType;
import com.example.model.exceptions.MyException;
import com.example.model.PrgState;
import com.example.model.exceptions.StmtException;
import com.example.model.expressions.Exp;


public class CondAssignStmt implements IStmt {
    private String var;
    private Exp exp1;
    private Exp exp2;
    private Exp exp3;

    public CondAssignStmt(String var, Exp exp1, Exp exp2, Exp exp3) {
        this.var = var;
        this.exp1 = exp1;
        this.exp2 = exp2;
        this.exp3 = exp3;
    }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        if (!typeEnv.containsKey(var)) { throw new MyException("CondAssign statement: Variable is not defined"); }

        Type varType = typeEnv.get(var);
        Type exp1Type = exp1.typeCheck(typeEnv);
        Type exp2Type = exp2.typeCheck(typeEnv);
        Type exp3Type = exp3.typeCheck(typeEnv);
        
        if (!exp1Type.equals(new BoolType())) { throw new MyException("CondAssign statement: Expression 1 is not a boolean"); }
        if (!varType.equals(exp2Type) || !varType.equals(exp3Type)) { throw new MyException("CondAssign statement: Expressions do not have the same type"); }

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException, StmtException {
        var symTbl = prg.getSymTable();
        if (!symTbl.containsKey(var)) { throw new StmtException("CondAssign statement: Variable is not defined"); }

        var exeStack = prg.getExeStack();
        IfStmt ifStmt = new IfStmt(exp1, new AssignStmt(var, exp2), new AssignStmt(var, exp3));
        exeStack.push(ifStmt);

        return null;
    }

    @Override
    public IStmt deepCopy() {
        return new CondAssignStmt(var, exp1.deepCopy(), exp2.deepCopy(), exp3.deepCopy());
    }

    @Override
    public String toString() {
        return var + " = " + exp1.toString() + " ? " + exp2.toString() + " : " + exp3.toString();
    }

}

