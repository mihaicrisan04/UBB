package repository;

public class CapacityExceededException extends Exception {
    public CapacityExceededException(String message) {
        super(message);
    }
    public CapacityExceededException() { super("Capacity exceeded"); }
}
