package com.example.model.values;

import com.example.model.types.Type;
import com.example.model.types.BoolType;

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
    public boolean equals(Object another) {
        if (another instanceof BoolValue other) {
            return value == other.value;
        }
        // if (another instanceof BoolValue) {
        //     BoolValue other = (BoolValue) another;
        //     return value == other.value;
        // }
        return false;
    }

    @Override
    public Type getType() {
        return new BoolType();
    }

    @Override
    public Value deepCopy() {
        return new BoolValue(value);
    }

    @Override
    public String toString() {
        return Boolean.toString(value);
    }
}
