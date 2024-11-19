package model.expressions;

import collections.dictionary.MyIDictionary;
import collections.heap.MyIHeap;
import model.values.Value;
import model.exceptions.MyException;

public interface Exp {
    Value eval(MyIDictionary<String, Value> table, MyIHeap<Integer, Value> heap) throws MyException;
    Exp deepCopy();
}