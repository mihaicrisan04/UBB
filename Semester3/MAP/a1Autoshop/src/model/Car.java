package model;

public class Car extends AbstractVehicle {
    private int numberOfDoors;

    public Car() {
        super();
        numberOfDoors = 0;
    }

    public Car(String name, int repairPrice, int numberOfDoors) {
        super(name, repairPrice);
        this.numberOfDoors = numberOfDoors;
    }

    public int getNumberOfDoors() {
        return numberOfDoors;
    }

    public void setNumberOfDoors(int numberOfDoors) {
        this.numberOfDoors = numberOfDoors;
    }

    @Override
    public String toString() {
        return super.toString() + " " + numberOfDoors;
    }
}
