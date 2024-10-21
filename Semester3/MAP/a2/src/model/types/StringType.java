package model.types;

public class StringType implements Type {
    @Override
    public boolean equals(Object other) {
        return other instanceof StringType;
    }

    @Override
    public String toString() {
        return "string";
    }
}
