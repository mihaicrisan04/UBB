package model;

public class Apple extends Tree {
    private String breed;

    public Apple(String name, int age, String breed) {
        super(name, age);
        this.breed = breed;
    }

    public Apple() {
        super("Apple", 0);
        this.breed = "Unknown";
    }

    @Override
    public String toString() {
        return super.toString() + ", Breed: " + breed;
    }
}
