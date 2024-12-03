package model.statements.file;

import java.io.BufferedReader;
import java.io.IOException;

import collections.heap.MyIHeap;
import collections.dictionary.MyIDictionary;
import model.exceptions.MyException;
import model.exceptions.StmtException;
import model.statements.IStmt;
import model.types.Type;
import model.types.IntType;
import model.PrgState;
import model.expressions.Exp;
import model.values.Value;
import model.values.StringValue;
import model.types.StringType;
import model.values.IntValue;

public class ReadFileStmt implements IStmt {
    private Exp exp;
    private String varName;

    public ReadFileStmt(Exp exp, String varName) {
        this.exp = exp;
        this.varName = varName;
    }

    public String getVarName() { return varName; }
    public Exp getExp() { return exp; }

    @Override
    public MyIDictionary<String, Type> typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        Type expType = exp.typeCheck(typeEnv);
        if (!expType.equals(new StringType())) { throw new MyException("Read File Stmt: expression is not a string"); }

        if (!typeEnv.containsKey(varName)) { throw new MyException("Read File Stmt: variable " + varName + " is not defined"); }

        // TODO: only be able to read integers for now
        if (!typeEnv.get(varName).equals(new IntType())) { throw new MyException("Read File Stmt: variable " + varName + " is not an integer"); }

        return typeEnv;
    }

    @Override
    public PrgState execute(PrgState state) throws MyException {
        MyIDictionary<String, Value> symTable = state.getSymTable();
        MyIHeap<Integer, Value> heapTable = state.getHeap();

        if (!symTable.containsKey(varName)) { throw new StmtException("Variable not declared"); }
        Value varVal = symTable.get(varName);
        if (!varVal.getType().equals(new IntType())) { throw new StmtException("Variable is not an integer"); }

        Value val;
        try { val = exp.eval(symTable, heapTable); }
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
        return new ReadFileStmt(exp.deepCopy(), varName);
    }

    @Override
    public String toString() {
        return "readFile(" + exp.toString() + ", " + varName + ")";
    }
}
