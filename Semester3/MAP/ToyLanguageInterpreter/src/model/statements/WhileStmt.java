package model.statements;

import model.exceptions.MyException;
import model.expressions.Exp;
import model.values.BoolValue;
import model.PrgState;
import model.types.BoolType;
import model.values.Value;


public class WhileStmt implements IStmt {
    private Exp exp;
    private IStmt stmt;

    public WhileStmt(Exp exp, IStmt stmt) {
        this.exp = exp;
        this.stmt = stmt;
    }

    @Override
    public PrgState execute(PrgState state) throws MyException {
        var symTbl = state.getSymTable();
        var heap = state.getHeap();

        Value val = exp.eval(symTbl, heap);
        if (!val.getType().equals(new BoolType())) {
            throw new MyException("Expression is not a boolean");
        }

        BoolValue boolVal = (BoolValue) val;
        if (boolVal.getValue()) {
            state.getExeStack().push(this);
            state.getExeStack().push(stmt);
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
