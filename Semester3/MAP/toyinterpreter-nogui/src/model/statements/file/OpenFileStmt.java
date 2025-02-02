package model.statements.file;

import collections.dictionary.MyIDictionary;
import model.exceptions.MyException;
import model.exceptions.StmtException;
import model.statements.IStmt;
import model.PrgState;
import model.types.Type;
import model.values.StringValue;
import model.values.Value;
import model.types.StringType;
import model.expressions.Exp;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;


public class OpenFileStmt implements IStmt {
    private Exp exp;

    public OpenFileStmt(Exp exp) {
        this.exp = exp;
    }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        Type expType = exp.typeCheck(typeEnv);
        if (!expType.equals(new StringType())) { throw new MyException("Open File Stmt: expression is not a string"); }

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState state) throws MyException, StmtException {
        Value val;
        try { val = exp.eval(state.getSymTable(), state.getHeap()); }
        catch (MyException e) { throw new StmtException("Error evaluating expression: " + e.getMessage()); }

        if (!val.getType().equals(new StringType())) { throw new StmtException("Expression is not a string"); }

        MyIDictionary<StringValue, BufferedReader> fileTable = state.getFileTable();
        StringValue file = (StringValue) val;

        if (fileTable.containsKey(file)) { throw new StmtException("File already opened"); }

        try {
            BufferedReader reader = new BufferedReader(new FileReader(file.getValue()));
            fileTable.put(file, reader);
        } catch (FileNotFoundException e) {
            throw new StmtException("File not found");
        }

        return state;
    }

    @Override
    public IStmt deepCopy() {
        return new OpenFileStmt(exp.deepCopy());
    }

    @Override
    public String toString() {
        return "open(" + exp.toString() + ")";
    }
}
