package com.example.model.expressions;

import com.example.model.exceptions.DivisionByZeroException;
import com.example.model.exceptions.ExpException;
import com.example.model.exceptions.InvalidOperandType;
import com.example.model.exceptions.MyException;
import com.example.model.types.Type;
import com.example.model.types.IntType;
import com.example.model.values.IntValue;
import com.example.model.values.Value;
import com.example.model.enums.ArithOperation;
import com.example.collections.dictionary.MyIDictionary;
import com.example.collections.heap.MyIHeap;

public class ArithExp implements Exp {
    Exp e1, e2;    
    ArithOperation op;

    public ArithExp(Exp e1, Exp e2, ArithOperation op) {
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

        return new IntType();
    }

    @Override
    public Value eval(MyIDictionary<String, Value> table, MyIHeap<Integer, Value> heap) throws ExpException, DivisionByZeroException, InvalidOperandType {
        Value v1, v2;
        try {
            v1 = e1.eval(table, heap);
            v2 = e2.eval(table, heap);
        } catch (MyException e) { throw new ExpException(e.getMessage()); }

        if (!v1.getType().equals(new IntType())) { throw new InvalidOperandType("First operand is not an integer"); }
        if (!v2.getType().equals(new IntType())) { throw new InvalidOperandType("Second operand is not an integer"); }

        IntValue i1 = (IntValue)v1;
        IntValue i2 = (IntValue)v2;
        int n1 = i1.getValue();
        int n2 = i2.getValue();

        switch (op) {
            case ADD:
                return new IntValue(n1 + n2);
            case SUB:
                return new IntValue(n1 - n2);
            case MUL:
                return new IntValue(n1 * n2);
            case DIV:
                if (n2 == 0) throw new DivisionByZeroException("Division by zero");
                return new IntValue(n1 / n2);
            default:
                throw new ExpException("Invalid operation");
        }
    }

    @Override
    public Exp deepCopy() {
        return new ArithExp(e1.deepCopy(), e2.deepCopy(), op);
    }

    @Override
    public String toString() {
        return "(" + e1.toString() + " " + op.toString() + " " + e2.toString() + ")";
    }
}


