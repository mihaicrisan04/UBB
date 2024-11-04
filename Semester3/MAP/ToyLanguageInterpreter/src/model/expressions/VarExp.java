package model.expressions;

import model.exceptions.VariableNotDefined;
import model.values.Value;
import collections.dictionary.MyIDictionary;


public class VarExp implements Exp {
    private String id;

    public VarExp(String id) {
        this.id = id;
    }

    @Override
    public Value eval(MyIDictionary<String, Value> table) throws VariableNotDefined{
        if (!table.containsKey(id)) { throw new VariableNotDefined("Variable " + id + " is not defined"); }
        return table.get(id);
    }

    @Override
    public Exp deepCopy() {
        return new VarExp(id);
    }

    @Override
    public String toString() {
        return id;
    }
}
