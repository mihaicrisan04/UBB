package model.expressions;

import model.exceptions.MyException;
import model.exceptions.VariableNotDefined;
import model.values.Value;
import model.types.Type;
import collections.dictionary.MyIDictionary;
import collections.heap.MyIHeap;


public class VarExp implements Exp {
    private String id;

    public VarExp(String id) {
        this.id = id;
    }

    @Override
    public Type typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        if (!typeEnv.containsKey(id)) { throw new VariableNotDefined("Variable " + id + " is not defined"); }
        return typeEnv.get(id);
    }

    @Override
    public Value eval(MyIDictionary<String, Value> table, MyIHeap<Integer, Value> heap) throws VariableNotDefined{
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
