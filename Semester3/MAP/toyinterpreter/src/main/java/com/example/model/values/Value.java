package com.example.model.values;

import com.example.model.types.Type;

public interface Value {
    Type getType();
    String toString();
    Value deepCopy();
}
