package com.example.model.expressions;

import com.example.model.values.Value;
import com.example.model.exceptions.MyException;
import com.example.model.types.Type;
import com.example.collections.dictionary.MyIDictionary;
import com.example.collections.heap.MyIHeap;

public class ValueExp implements Exp {
    private Value val;

    public ValueExp(Value v) {
        val = v;
    }

    @Override
    public Type typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        return val.getType();
    }

    @Override
    public Value eval(MyIDictionary<String, Value> table, MyIHeap<Integer, Value> heap) throws MyException {
        return val;
    }

    @Override
    public Exp deepCopy() {
        return new ValueExp(val.deepCopy());
    }

    @Override
    public String toString() {
        return val.toString();
    }
}
