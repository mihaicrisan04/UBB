package com.example.model.types;

import com.example.model.values.StringValue;
import com.example.model.values.Value;

public class StringType implements Type {
    @Override
    public boolean equals(Object other) {
        return other instanceof StringType;
    }

    @Override
    public Value defaultValue() {
        return new StringValue("");
    }

    @Override
    public String toString() {
        return "string";
    }
}
