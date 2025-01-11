package com.example.model.statements;

import com.example.collections.dictionary.MyIDictionary;
import com.example.collections.stack.MyIStack;
import com.example.model.PrgState;
import com.example.model.types.Type;
import com.example.model.exceptions.MyException;

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
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        return second.typeCheck(first.typeCheck(typeEnv));
    }

    @Override
    public PrgState execute(PrgState prg) throws MyException {
        MyIStack<IStmt> stack = prg.getExeStack();
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
        return first.toString() + ";\n" + second.toString();
        // return "{" + first.toString() + "; " + second.toString() + "}";
    }
}
