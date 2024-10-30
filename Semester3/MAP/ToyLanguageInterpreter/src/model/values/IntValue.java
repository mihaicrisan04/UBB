package model.values;

import model.types.Type;
import model.types.IntType;

public class IntValue implements Value {
    private int value;

    public IntValue() {
        this.value = 0;
    }

    public IntValue(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    @Override
    public boolean equals(Object another) {
        if (another instanceof IntValue) {
            return value == ((IntValue) another).getValue();
        }
        return false;
    }

    @Override
    public Type getType() {
        return new IntType();
    }

    @Override
    public Value deepCopy() {
        return new IntValue(value);
    }

    @Override
    public String toString() {
        return Integer.toString(value);
    }
}
