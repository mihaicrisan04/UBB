package model.expressions;

import model.exceptions.MyException;
import model.types.IntType;
import model.values.IntValue;
import model.values.Value;
import collections.dictionary.MyIDictionary;
import model.enums.ArithOperation;

public class ArithExp implements Exp {
    Exp e1, e2;    
    ArithOperation op;

    public ArithExp(Exp e1, Exp e2, ArithOperation op) {
        this.e1 = e1;
        this.e2 = e2;
        this.op = op;
    }

    @Override
    public Value eval(MyIDictionary<String, Value> table) throws MyException {
        Value v1, v2;

        v1 = e1.eval(table);
        if (!v1.getType().equals(new IntType())) { throw new MyException("First operand is not an integer"); }
        v2 = e2.eval(table);
        if (!v2.getType().equals(new IntType())) { throw new MyException("Second operand is not an integer"); }

        IntValue i1 = (IntValue)v1;
        IntValue i2 = (IntValue)v2;
        int n1 = i1.getValue();
        int n2 = i2.getValue();

        switch (op) {
            case ADD:
                return new IntValue(n1 + n2);
            case SUBTRACT:
                return new IntValue(n1 - n2);
            case MULTIPLY:
                return new IntValue(n1 * n2);
            case DIVIDE:
                if (n2 == 0) throw new MyException("Division by zero");
                return new IntValue(n1 / n2);
            default:
                throw new MyException("Invalid operation");
        }
    }

    @Override
    public String toString() {
        return e1.toString() + " " + op.toString() + " " + e2.toString();
    }
}


