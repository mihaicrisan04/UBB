package model.expressions;

import collections.dictionary.MyIDictionary;
import collections.heap.MyIHeap;
import model.enums.CompareOperation;
import model.exceptions.ExpException;
import model.exceptions.InvalidOperandType;
import model.exceptions.MyException;
import model.types.Type;
import model.types.IntType;
import model.values.Value;
import model.values.BoolValue;
import model.values.IntValue;
import model.types.BoolType;


public class CompareExp implements Exp {
    private Exp e1, e2;
    private CompareOperation op;

    public CompareExp(Exp e1, Exp e2, CompareOperation op) {
        this.e1 = e1;
        this.e2 = e2;
        this.op = op;
    }

    @Override
    public Type typeCheck(MyIDictionary<String, Type> typeEnv) throws MyException {
        Type t1 = e1.typeCheck(typeEnv);
        Type t2 = e2.typeCheck(typeEnv);

        if (!t1.equals(new IntType())) { throw new InvalidOperandType("First operand is not an integer"); }
        if (!t2.equals(new IntType())) { throw new InvalidOperandType("Second operand is not an integer"); }

        return new BoolType();
    }

    @Override
    public Value eval(MyIDictionary<String, Value> table, MyIHeap<Integer, Value> heap) throws MyException, ExpException, InvalidOperandType {
        Value v1, v2;
        try {
            v1 = e1.eval(table, heap);
            v2 = e2.eval(table, heap);
        } catch (MyException e) {
            throw new MyException("Compare expression: " + e.getMessage());
        }

        if (!v1.getType().equals(new IntType())) { throw new InvalidOperandType("First operand is not an integer"); }
        if (!v2.getType().equals(new IntType())) { throw new InvalidOperandType("Second operand is not an integer"); }

        IntValue i1 = (IntValue) v1;
        IntValue i2 = (IntValue) v2;
        int n1 = i1.getValue();
        int n2 = i2.getValue();

        switch (op) {
            case EQUAL: return new BoolValue(n1 == n2);
            case NOT_EQUAL: return new BoolValue(n1 != n2);
            case LESS: return new  BoolValue(n1 < n2);           
            case LESS_OR_EQUAL: return new BoolValue(n1 <= n2);
            case GREATER: return new  BoolValue(n1 > n2);           
            case GREATER_OR_EQUAL: return new BoolValue(n1 >= n2);
            default: throw new ExpException("Invalid operation");
        }
    }

    @Override
    public Exp deepCopy() {
        return new CompareExp(e1.deepCopy(), e2.deepCopy(), op);
    }

    @Override
    public String toString() {
        return e1.toString() + " " + op.toString() + " " + e2.toString();
    }
}
