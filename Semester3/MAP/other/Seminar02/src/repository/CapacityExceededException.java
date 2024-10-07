package repository;

public class CapacityExceededException extends Exception {

    public CapacityExceededException(){
        super();
    }

    public CapacityExceededException(String message){
        super(message);
    }
}
