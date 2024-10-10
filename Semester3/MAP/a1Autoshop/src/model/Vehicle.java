package model;

public abstract class Vehicle {
    private String name;
    private int repairPrice;

    public Vehicle() {
        name = "";
        repairPrice = 0;
    }

    public Vehicle(String name, int repairPrice) {
        this.name = name;
        this.repairPrice = repairPrice;
    }

    public String getName() {
        return name;
    }

    public int getRepairPrice() {
        return repairPrice;
    }

    public void setRepairPrice(int repairPrice) {
        this.repairPrice = repairPrice;
    }

    public String toString() {
        return name + " " + repairPrice;
    }
}
