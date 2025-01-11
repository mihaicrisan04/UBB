package com.example.model.expressions;

import com.example.collections.dictionary.MyIDictionary;
import com.example.collections.heap.MyIHeap;
import com.example.model.values.Value;
import com.example.model.exceptions.MyException;
import com.example.model.types.Type;

public interface Exp {
    Type typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException;
    Value eval(MyIDictionary<String, Value> table, MyIHeap<Integer, Value> heap) throws MyException;
    Exp deepCopy();
}