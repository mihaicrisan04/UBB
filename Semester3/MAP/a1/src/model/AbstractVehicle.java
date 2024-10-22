package model;

public abstract class AbstractVehicle implements Vehicle {
    private String name;
    private int repairPrice;

    public AbstractVehicle() {
        name = "";
        repairPrice = 0;
    }

    public AbstractVehicle(String name, int repairPrice) {
        this.name = name;
        this.repairPrice = repairPrice;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public int getRepairPrice() {
        return repairPrice;
    }

    @Override
    public void setRepairPrice(int repairPrice) {
        this.repairPrice = repairPrice;
    }

    @Override
    public String toString() {
        return name + " " + repairPrice;
    }
}
