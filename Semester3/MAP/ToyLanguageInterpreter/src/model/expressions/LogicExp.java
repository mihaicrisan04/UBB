package model.expressions;

import model.enums.LogicOperation;
import model.exceptions.ExpException;
import model.exceptions.InvalidOperandType;
import model.exceptions.MyException;
import model.types.BoolType;
import model.values.BoolValue;
import model.values.Value;
import collections.dictionary.MyIDictionary;


public class LogicExp implements Exp {
    Exp e1, e2;
    LogicOperation op;

    public LogicExp(Exp e1, Exp e2, LogicOperation op) {
        this.e1 = e1;
        this.e2 = e2;
        this.op = op;
    }

    @Override
    public Value eval(MyIDictionary<String, Value> table) throws MyException, InvalidOperandType, ExpException {
        Value v1, v2;
        try {
            v1 = e1.eval(table);
            v2 = e2.eval(table);
        } catch (MyException e) { throw new ExpException("Logic expression: " + e.getMessage()); }

        if (!v1.getType().equals(new BoolType())) { throw new InvalidOperandType("First operand is not a boolean"); }
        if (!v2.getType().equals(new BoolType())) { throw new InvalidOperandType("Second operand is not a boolean"); }

        BoolValue b1 = (BoolValue)v1;
        BoolValue b2 = (BoolValue)v2;
        boolean n1 = b1.getValue();
        boolean n2 = b2.getValue();

        switch(op) {
            case AND: return new BoolValue(n1 && n2);
            case OR: return new BoolValue(n1 || n2);
            default: throw new ExpException("Invalid operation");
        }
    }

    @Override
    public Exp deepCopy() {
        return new LogicExp(e1.deepCopy(), e2.deepCopy(), op);
    }

    @Override
    public String toString() {
        return e1.toString() + " " + op.toString() + " " + e2.toString();
    }
}
