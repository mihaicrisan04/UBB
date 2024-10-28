package model.types;

import model.values.Value;

public interface Type {
    boolean equals(Object other);
    String toString();
    Value defaultValue();
}
