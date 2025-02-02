package com.example.model.values;

import com.example.model.types.Type;
import com.example.model.types.StringType;

public class StringValue implements Value {
    private String value;

    public StringValue() {
        this.value = "";
    }

    public StringValue(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    @Override
    public boolean equals(Object another) {
        if (another instanceof StringValue other) {
            return value.equals(other.value);
        }
        // if (another instanceof StringValue) {
        //     StringValue other = (StringValue) another;
        //     return value.equals(other.value);
        // }
        return false;
    }

    @Override
    public Type getType() {
        return new StringType();
    }

    @Override
    public Value deepCopy() {
        return new StringValue(value);
    }

    @Override
    public String toString() {
        return value;
    }
}
