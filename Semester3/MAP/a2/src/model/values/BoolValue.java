package model.values;

import model.types.Type;
import model.types.BoolType;

public class BoolValue implements Value {
    private boolean value;

    public BoolValue() {
        this.value = false;
    }

    public BoolValue(boolean value) {
        this.value = value;
    }

    public boolean getValue() {
        return value;
    }

    @Override
    public Type getType() {
        return new BoolType();
    }

    @Override
    public String toString() {
        return Boolean.toString(value);
    }
}
