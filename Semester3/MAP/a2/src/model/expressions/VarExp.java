package model.expressions;

import model.exceptions.MyException;
import model.values.Value;
import collections.dictionary.MyIDictionary;


public class VarExp implements Exp {
    private String id;

    public VarExp(String id) {
        this.id = id;
    }

    @Override
    public Value eval(MyIDictionary<String, Value> table) throws MyException {
        if (!table.containsKey(id)) {
            throw new MyException("Variable " + id + " is not defined");
        }
        return table.get(id);
    }

    @Override
    public String toString() {
        return id;
    }
}
