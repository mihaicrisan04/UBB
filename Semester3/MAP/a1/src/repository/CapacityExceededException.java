package repository;

public class CapacityExceededException extends RuntimeException {
    public CapacityExceededException(String message) {
        super(message);
    }
    public CapacityExceededException() { super("Capacity exceeded"); }
}
