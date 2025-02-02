package com.example.model.types;

import com.example.model.values.Value;

public interface Type {
    boolean equals(Object other);
    String toString();
    Value defaultValue();
}
