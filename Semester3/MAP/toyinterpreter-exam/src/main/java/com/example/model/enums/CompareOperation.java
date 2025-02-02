package com.example.model.enums;

public enum CompareOperation {
    EQUAL("=="),
    NOT_EQUAL("!="),
    LESS("<"),
    LESS_OR_EQUAL("<="),
    GREATER(">"),
    GREATER_OR_EQUAL(">=");

    private final String symbol;

    CompareOperation(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return symbol;
    }
}

