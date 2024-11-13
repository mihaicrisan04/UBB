package model.statements.files;

import java.io.BufferedReader;
import java.io.IOException;

import model.exceptions.MyException;
import model.exceptions.StmtException;
import model.statements.IStmt;
import model.types.IntType;
import collections.dictionary.MyIDictionary;
import model.PrgState;
import model.expressions.Exp;
import model.values.Value;
import model.values.StringValue;
import model.types.StringType;
import model.values.IntValue;

public class ReadStmt implements IStmt {
    private Exp exp;
    private String varName;

    public ReadStmt(Exp exp, String varName) {
        this.exp = exp;
        this.varName = varName;
    }

    public String getVarName() { return varName; }

    public Exp getExp() { return exp; }

    @Override
    public PrgState execute(PrgState state) throws MyException {
        MyIDictionary<String, Value> symTable = state.getSymTable();

        if (!symTable.containsKey(varName)) { throw new StmtException("Variable not declared"); }
        Value varVal = symTable.get(varName);
        if (!varVal.getType().equals(new IntType())) { throw new StmtException("Variable is not an integer"); }

        Value val;
        try { val = exp.eval(symTable); }
        catch (MyException e) { throw new StmtException("Error evaluating expression: " + e.getMessage()); }

        if (!val.getType().equals(new StringType())) { throw new StmtException("Expression is not a string"); }

        MyIDictionary<StringValue, BufferedReader> fileTable = state.getFileTable();
        StringValue file = (StringValue) val;

        if (!fileTable.containsKey(file)) { throw new StmtException("File is not opened"); }

        BufferedReader reader = fileTable.get(file);
        try {
            String line = reader.readLine();
            // if (line == null) { throw new StmtException("End of file reached"); }
            if (line == null) {
                symTable.put(varName, new IntValue(0));
                return state;
            }
            int number = Integer.parseInt(line);
            symTable.put(varName, new IntValue(number));
        } catch (IOException e) {
            throw new StmtException("Error reading from file");
        }

        return state;
    }

    @Override
    public IStmt deepCopy() {
        return new ReadStmt(exp.deepCopy(), varName);
    }

    @Override
    public String toString() {
        return "read(" + exp.toString() + ", " + varName + ")";
    }
}
