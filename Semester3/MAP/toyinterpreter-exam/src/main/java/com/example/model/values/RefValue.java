package com.example.model.values;

import com.example.model.types.RefType;
import com.example.model.types.Type;


public class RefValue implements Value {
    private int address;
    private Type locationType;

    public RefValue(int address, Type locationType) {
        this.address = address;
        this.locationType = locationType;
    }

    public int getAddress() { return address; }
    public Type getLocationType() { return locationType; }

    public void setAddress(int address) { this.address = address; }

    @Override
    public boolean equals(Object other) {
        if (other instanceof RefValue) {
            RefValue otherRef = (RefValue) other;
            return address == otherRef.getAddress() && locationType.equals(otherRef.getLocationType());
        }
        return false;
    }

    @Override
    public String toString() {
        return "(" + address + ", " + locationType.toString() + ")";
    }

    @Override
    public Type getType() {
        return new RefType(locationType);
    }

    @Override
    public Value deepCopy() {
        return new RefValue(address, locationType);
    }
}
