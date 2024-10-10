package model;

public class Truck extends Vehicle {
    private int maxWeight;

    public Truck() {
        super();
        maxWeight = 0;
    }

    public Truck(String name, int repairPrice, int maxWeight) {
        super(name, repairPrice);
        this.maxWeight = maxWeight;
    }

    public int getMaxWeight() {
        return maxWeight;
    }

    public void setMaxWeight(int maxWeight) {
        this.maxWeight = maxWeight;
    }

    @Override
    public String toString() {
        return super.toString() + " " + maxWeight;
    }
}
