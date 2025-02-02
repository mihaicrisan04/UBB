package model.expressions;

import collections.dictionary.MyIDictionary;
import collections.heap.MyIHeap;
import model.values.Value;
import model.exceptions.MyException;
import model.types.Type;

public interface Exp {
    Type typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException;
    Value eval(MyIDictionary<String, Value> table, MyIHeap<Integer, Value> heap) throws MyException;
    Exp deepCopy();
}