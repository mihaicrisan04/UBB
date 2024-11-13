package model.statements;

import collections.stack.MyIStack;
import model.PrgState;
import model.exceptions.MyException;

public class CompoundStmt implements IStmt {
    private IStmt first;
    private IStmt second;

    public CompoundStmt(IStmt first, IStmt second) {
        this.first = first;
        this.second = second;
    }

    public IStmt getFirst() { return first; }

    public IStmt getSecond() { return second; }

    @Override
    public PrgState execute(PrgState state) throws MyException {
        MyIStack<IStmt> stack = state.getExeStack();
        stack.push(second);
        stack.push(first);
        return null;
    }

    @Override
    public IStmt deepCopy() {
        return new CompoundStmt(first.deepCopy(), second.deepCopy());
    }

    @Override
    public String toString() {
        // return first.toString() + ";\n" + second.toString();
        return "{" + first.toString() + "; " + second.toString() + "}";
    }
}
