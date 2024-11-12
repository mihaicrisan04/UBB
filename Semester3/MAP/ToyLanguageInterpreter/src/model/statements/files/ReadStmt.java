package model.statements.files;

import model.exceptions.MyException;
import model.statements.IStmt;
import model.PrgState;

public class ReadStmt implements IStmt {
    private String varName;
    private String fileName;

    public ReadStmt(String varName, String fileName) {
        this.varName = varName;
        this.fileName = fileName;
    }

    public String getVarName() {
        return varName;
    }

    public String getFileName() {
        return fileName;
    }

    @Override
    public PrgState execute(PrgState state) throws MyException{
        // TODO: implement this

        return state;
    }

    @Override
    public IStmt deepCopy() {
        return new ReadStmt(varName, fileName);
    }

    @Override
    public String toString() {
        return "read(" + fileName + ", " + varName + ")";
    }
}
