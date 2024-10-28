package model.expressions;

import collections.dictionary.MyIDictionary;
import model.values.Value;
import model.exceptions.MyException;

public interface Exp {
    Value eval(MyIDictionary<String, Value> table) throws MyException;
    Exp deepCopy();
}