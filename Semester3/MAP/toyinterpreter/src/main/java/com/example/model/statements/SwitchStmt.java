package com.example.model.statements;


import com.example.collections.dictionary.MyIDictionary;
import com.example.model.expressions.Exp;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.model.exceptions.StmtException;
import com.example.model.types.Type;
import com.example.model.values.IntValue;
import com.example.model.values.Value;


// switch(exp) (case exp1: stmt1) (case exp2: stmt2) (default: stmt3)
public class SwitchStmt implements IStmt {
    private Exp exp;
    private Exp exp1;
    private Exp exp2;
    private IStmt stmt1;
    private IStmt stmt2;
    private IStmt stmt3;
    
    public SwitchStmt(Exp exp, Exp exp1, IStmt stmt1, Exp exp2, IStmt stmt2, IStmt stmt3) {
        this.exp = exp;
        this.exp1 = exp1;
        this.exp2 = exp2;
        this.stmt1 = stmt1;
        this.stmt2 = stmt2;
        this.stmt3 = stmt3;
    }

    @Override 
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        Type typexp = exp.typeCheck(typeEnv);
        Type typexp1 = exp1.typeCheck(typeEnv);
        Type typexp2 = exp2.typeCheck(typeEnv);
        if (!typexp.equals(typexp1) || !typexp.equals(typexp2)) { throw new MyException("Switch statement: Expressions do not have the same type"); }
        
        stmt1.typeCheck(typeEnv.deepCopy());
        stmt2.typeCheck(typeEnv.deepCopy());
        stmt3.typeCheck(typeEnv.deepCopy());
        
        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException, StmtException {
        Value val;
        Value val1;
        Value val2;

        try { val = exp.eval(prg.getSymTable(), prg.getHeap()); }
        catch (MyException e) { throw new StmtException("Switch statement: " + e.getMessage()); }

        try { val1 = exp1.eval(prg.getSymTable(), prg.getHeap()); }
        catch (MyException e) { throw new StmtException("Switch statement: " + e.getMessage()); }

        try { val2 = exp2.eval(prg.getSymTable(), prg.getHeap()); }
        catch (MyException e) { throw new StmtException("Switch statement: " + e.getMessage()); }

        IntValue i = (IntValue) val;
        IntValue i1 = (IntValue) val1;
        IntValue i2 = (IntValue) val2;

        if (i.getValue() == i1.getValue()) { prg.getExeStack().push(stmt1); }
        else if (i.getValue() == i2.getValue()) { prg.getExeStack().push(stmt2); }
        else { prg.getExeStack().push(stmt3); }

        return null;
    }

    @Override
    public IStmt deepCopy() {
        return new SwitchStmt(exp.deepCopy(), exp1.deepCopy(), stmt1.deepCopy(), exp2.deepCopy(), stmt2.deepCopy(), stmt3.deepCopy());
    }
    
    @Override
    public String toString() {
        return "SWITCH(" + exp.toString() + ") (CASE(" + exp1.toString() + "): " + stmt1.toString() + ") (CASE(" + exp2.toString() + "): " + stmt2.toString() + ") (DEFAULT: " + stmt3.toString() + ")";
    }
}
