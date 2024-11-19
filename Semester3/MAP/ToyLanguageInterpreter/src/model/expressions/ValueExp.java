package model.expressions;

import model.values.Value; 
import model.exceptions.MyException;
import collections.dictionary.MyIDictionary;
import collections.heap.MyIHeap;

public class ValueExp implements Exp {
    private Value val;

    public ValueExp(Value v) {
        val = v;
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
