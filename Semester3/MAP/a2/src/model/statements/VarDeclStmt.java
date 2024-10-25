package model.statements;

import java.lang.foreign.ValueLayout;

import collections.dictionary.MyIDictionary;
import model.PrgState;
import model.exceptions.MyException;
import model.values.*;
import model.types.*;

public class VarDeclStmt implements IStmt {
    String id;
    Type type;

    public VarDeclStmt(String id, Type type) {
        this.id = id;
        this.type = type;
    }

    @Override
    public PrgState execute(PrgState state) throws MyException {
        MyIDictionary<String, Value> symTable = state.getSymTable();

        if (symTable.containsKey(id)) { throw new MyException("Variable " + id + " is already defined"); }

        if (type.equals(new IntType())) { symTable.put(id, new IntValue(0)); }
        else if (type.equals(new BoolType())) { symTable.put(id, new BoolValue(false)); }
        else if (type.equals(new StringType())) { symTable.put(id, new StringValue("")); }

        return state;
    }

    @Override
    public String toString() {
        return type.toString() + " " + id;
    }
    
}
