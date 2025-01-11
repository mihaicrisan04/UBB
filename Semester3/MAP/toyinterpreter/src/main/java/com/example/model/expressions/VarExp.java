package com.example.model.expressions;

import com.example.model.exceptions.MyException;
import com.example.model.exceptions.VariableNotDefined;
import com.example.model.values.Value;
import com.example.model.types.Type;
import com.example.collections.dictionary.MyIDictionary;
import com.example.collections.heap.MyIHeap;


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
