package model.expressions;

import model.values.Value; 
import model.exceptions.MyException;
import model.types.Type;
import collections.dictionary.MyIDictionary;
import collections.heap.MyIHeap;

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
