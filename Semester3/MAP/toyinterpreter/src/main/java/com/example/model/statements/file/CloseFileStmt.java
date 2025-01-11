package com.example.model.statements.file;

import com.example.collections.dictionary.MyIDictionary;
import com.example.model.statements.IStmt;
import com.example.model.types.Type;
import com.example.model.types.StringType;
import com.example.model.values.StringValue;
import com.example.model.values.Value;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.model.exceptions.StmtException;
import com.example.model.expressions.Exp;

import java.io.BufferedReader;
import java.io.FileNotFoundException;


public class CloseFileStmt implements IStmt {
    private Exp exp;

    public CloseFileStmt(Exp exp) {
        this.exp = exp;
    }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        Type expType = exp.typeCheck(typeEnv);
        if (!expType.equals(new StringType())) { throw new MyException("Close File Stmt: expression is not a string"); }

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState state) throws MyException {
        Value val;
        try { val = exp.eval(state.getSymTable(), state.getHeap()); }
        catch (MyException e) { throw new StmtException("Error evaluating expression: " + e.getMessage()); }

        if (!val.getType().equals(new StringType())) { throw new StmtException("Expression is not a string"); }

        MyIDictionary<StringValue, BufferedReader> fileTable = state.getFileTable();
        StringValue file = (StringValue) val;

        if (!fileTable.containsKey(file)) { throw new StmtException("File is not opened"); }

        try {
            BufferedReader reader = fileTable.get(file);
            reader.close();
            fileTable.remove(file);
        } catch (FileNotFoundException e) {
            throw new StmtException("File not found");
        } catch (Exception e) {
            throw new StmtException("Error closing file");
        }

        return state;
    }

    @Override
    public IStmt deepCopy() {
        return new CloseFileStmt(exp.deepCopy());
    }

    @Override
    public String toString() {
        return "close(" + exp.toString() + ")";
    }
}
