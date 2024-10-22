package model.enums;

public enum LogicOperation {
    AND("&&"),
    OR("||");

    private final String symbol;

    LogicOperation(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return symbol;
    }
}

