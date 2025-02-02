package model.expressions;

import collections.dictionary.MyIDictionary;
import collections.heap.MyIHeap;
import model.exceptions.MyException;
import model.types.Type;
import model.types.RefType; 
import model.values.Value;
import model.values.RefValue;


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
