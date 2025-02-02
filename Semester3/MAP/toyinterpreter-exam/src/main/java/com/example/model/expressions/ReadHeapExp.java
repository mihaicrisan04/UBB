package com.example.model.expressions;

import com.example.collections.dictionary.MyIDictionary;
import com.example.collections.heap.MyIHeap;
import com.example.model.exceptions.MyException;
import com.example.model.types.Type;
import com.example.model.types.RefType; 
import com.example.model.values.Value;
import com.example.model.values.RefValue;


public class ReadHeapExp implements Exp {
    private Exp exp;
   
    public ReadHeapExp(Exp exp) {
        this.exp = exp;
    }

    @Override
    public Type typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        Type type = exp.typeCheck(typeEnv);
        if (!(type instanceof RefType)) { throw new MyException("Expression " + exp.toString() + " is not a reference"); }
        RefType refType = (RefType) type;
        return refType.getInner();
    }

    @Override
    public Value eval(MyIDictionary<String, Value> table, MyIHeap<Integer, Value> heap) throws MyException {
        Value val = exp.eval(table, heap);
        if (!(val instanceof RefValue)) { throw new MyException("Expression " + exp.toString() + " is not a reference"); }
        RefValue refVal = (RefValue) val;

        int address = refVal.getAddress();

        if (!heap.containsKey(address)) { throw new MyException("Address " + address + " is not defined in the heap"); }

        return heap.get(address);
    }
    
    @Override
    public String toString() {
        return "rH(" + exp + ")";
    }

    @Override
    public Exp deepCopy() {
        return new ReadHeapExp(exp);
    }
}
