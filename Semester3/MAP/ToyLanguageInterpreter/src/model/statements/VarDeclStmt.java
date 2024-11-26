package model.statements;

import collections.dictionary.MyIDictionary;
import model.PrgState;
import model.exceptions.VariableAlreadyDefined;
import model.values.Value;
import model.types.Type;

public class VarDeclStmt implements IStmt {
    String id;
    Type type;

    public VarDeclStmt(String id, Type type) {
        this.id = id;
        this.type = type;
    }

    @Override
    public PrgState execute(PrgState state) throws VariableAlreadyDefined {
        MyIDictionary<String, Value> symTable = state.getSymTable();

        if (symTable.containsKey(id)) { throw new VariableAlreadyDefined("Variable " + id + " is already defined"); }

        symTable.put(id, type.defaultValue());

        return null;
    }

    @Override
    public IStmt deepCopy() {
        return new VarDeclStmt(id, type);
    }

    @Override
    public String toString() {
        return type.toString() + " " + id;
    }
}
