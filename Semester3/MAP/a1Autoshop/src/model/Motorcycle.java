package model;

public class Motorcycle extends Vehicle {
    private boolean hasSidecar;

    public Motorcycle() {
        super();
        hasSidecar = false;
    }

    public Motorcycle(String name, int repairPrice, boolean hasSidecar) {
        super(name, repairPrice);
        this.hasSidecar = hasSidecar;
    }

    public boolean getHasSidecar() {
        return hasSidecar;
    }

    public void setHasSidecar(boolean hasSidecar) {
        this.hasSidecar = hasSidecar;
    }

    @Override
    public String toString() {
        return super.toString() + " " + hasSidecar;
    }
}  