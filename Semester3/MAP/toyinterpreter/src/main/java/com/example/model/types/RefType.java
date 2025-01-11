package com.example.model.types;

import com.example.model.values.RefValue;
import com.example.model.values.Value;


public class RefType implements Type {
    private Type inner;

    public RefType(Type inner) {
        this.inner = inner;
    }

    public Type getInner() { return inner; }

    @Override
    public boolean equals(Object other) {
        if (other instanceof RefType) {
            RefType otherRef = (RefType) other;
            return inner.equals(otherRef.getInner());
        }
        return false;
    }

    @Override
    public String toString() {
        return "Ref(" + inner.toString() + ")";
    }

    @Override
    public Value defaultValue() {
        return new RefValue(0, inner);
    }
}
