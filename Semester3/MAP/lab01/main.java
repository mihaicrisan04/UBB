class Main {    
    public static void main(String[] args) {
        // Create an instance of the superclass
        Animal animal = new Animal("Generic Animal");
        animal.makeSound();

        // Create an instance of the subclass
        Dog dog = new Dog("Buddy");
        dog.makeSound();

        // Use static fields and methods
        System.out.println("Number of animals: " + Animal.getCount());

        // Demonstrate exception handling
        try {
            Animal invalidAnimal = new Animal(null);
        } catch (IllegalArgumentException e) {
            System.out.println("Exception caught: " + e.getMessage());
        }

        if (args.length == 0) {
            System.out.println("No integers provided.");
            return;
        }

        int sum = 0;
        int count = 0;

        for (String arg : args) {
            try {
                int number = Integer.parseInt(arg);
                sum += number;
                count++;
            } catch (NumberFormatException e) {
                System.out.println("Invalid integer: " + arg);
                return;
            }
        }

        if (count > 0) {
            double average = (double) sum / count;
            System.out.println("Average: " + average);
        } else {
            System.out.println("No valid integers provided.");
        }
    }
}

class Animal {
    private String name;
    private static int count = 0;

    public Animal(String name) {
        if (name == null || name.isEmpty()) {
            throw new IllegalArgumentException("Name cannot be null or empty");
        }
        this.name = name;
        count++;
    }

    public void makeSound() {
        System.out.println(name + " makes a sound.");
    }

    public static int getCount() {
        return count;
    }
}

class Dog extends Animal {
    public Dog(String name) {
        super(name);
    }

    @Override
    public void makeSound() {
        System.out.println(getName() + " barks.");
    }

    private String getName() {
        return "Dog";
    }
}