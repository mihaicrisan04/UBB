package repository;

import model.Vehicle;

public class InMemoRepo implements Repo {

    private Vehicle[] v;
    private int capacity;

    public InMemoRepo() {
        v = new Vehicle[10];
        capacity = 10;
    }

    @Override
    public void add(Vehicle vehicle) {
        if (capacity == v.length) {
            Vehicle[] newV = new Vehicle[capacity * 2];
            capacity *= 2;
            v = newV;
        }
    }

    @Override
    public void remove(Vehicle vehicle) {

    }

    @Override
    public Vehicle[] getAll() {
        return new Vehicle[0];
    }
}
