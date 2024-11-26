package model.statements.file;

import model.statements.IStmt;
import model.types.StringType;
import model.values.StringValue;
import model.values.Value;
import collections.dictionary.MyIDictionary;
import model.PrgState;
import model.exceptions.MyException;
import model.exceptions.StmtException;
import model.expressions.Exp;

import java.io.BufferedReader;
import java.io.FileNotFoundException;


public class CloseFileStmt implements IStmt {
    private Exp exp;

    public CloseFileStmt(Exp exp) {
        this.exp = exp;
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
