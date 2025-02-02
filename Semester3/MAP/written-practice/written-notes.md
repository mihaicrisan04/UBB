# Written Notes Advanced Porgramming Methods

## **Interfaces vs Abstract Classes in Java**

Both **interfaces** and **abstract classes** are used to achieve abstraction in Java, but they have distinct differences and use cases.

### **1. Definition**
- **Interface**: A contract that defines methods without implementation (until Java 8+, where default and static methods are allowed).
- **Abstract Class**: A class that can have both abstract (unimplemented) and concrete (implemented) methods.

### **2. Key Differences**

| Feature                 | Interface                                            | Abstract Class                                       |
|-------------------------|------------------------------------------------------|-----------------------------------------------------|
| **Definition**           | Collection of abstract methods (until Java 8)       | A class that may contain abstract and concrete methods |
| **Method Implementation** | Methods are abstract by default (except Java 8+ default/static methods) | Can have fully implemented methods |
| **Fields (Variables)**   | Only `public static final` (constants)              | Can have instance variables with any access modifier |
| **Constructor**          | No constructors                                     | Can have constructors |
| **Multiple Inheritance** | Can be implemented by multiple classes              | A class can extend only one abstract class |
| **Access Modifiers**     | Methods are implicitly `public`                     | Methods can have any access modifier |
| **Usage**               | Best suited for defining a contract                 | Best for providing base functionality |

### **3. Example Code**

### **Interface Example**
```java
interface Animal {
    void makeSound(); // Abstract method
}

class Dog implements Animal {
    public void makeSound() {
        System.out.println("Bark!");
    }
}
```

### **Abstract Class Example**
```java
abstract class Animal {
    abstract void makeSound(); // Abstract method

    void sleep() {
        System.out.println("Sleeping...");
    }
}

class Dog extends Animal {
    void makeSound() {
        System.out.println("Bark!");
    }
}
```

---

## **Overriding vs Overloading in Java**  

Both **method overriding** and **method overloading** are techniques in Java that allow flexibility in defining methods, but they serve different purposes and have key differences.

### **1. Definition**  
- **Method Overloading**: Defining multiple methods with the same name but different parameter lists within the same class.  
- **Method Overriding**: Redefining a method from a parent class in a subclass to provide a specific implementation.

### **2. Key Differences**  

| Feature              | Method Overloading | Method Overriding |
|----------------------|-------------------|-------------------|
| **Definition**      | Same method name but different parameter lists | Subclass redefines a method from the parent class |
| **Parameters**      | Must be different (different type, number, or order of parameters) | Must be the same as the parent class method |
| **Return Type**     | Can be different | Must be the same or covariant (subtype of original return type) |
| **Access Modifier** | No restriction | Cannot reduce visibility (e.g., `public` method in parent cannot become `private` in child) |
| **Static Methods**  | Can be overloaded | Cannot be overridden (redeclaring a static method in a subclass is method hiding) |
| **Final Methods**   | Can be overloaded | Cannot be overridden |
| **Checked Exceptions** | Can have different checked exceptions | Cannot throw new or broader checked exceptions |
| **Polymorphism**    | Not related to runtime polymorphism | Supports runtime polymorphism |

### **3. Example Code**

### **Method Overloading**
```java
class MathUtils {
    int add(int a, int b) {
        return a + b;
    }

    double add(double a, double b) {
        return a + b;
    }

    int add(int a, int b, int c) {
        return a + b + c;
    }
}

class Test {
    public static void main(String[] args) {
        MathUtils math = new MathUtils();
        System.out.println(math.add(5, 10));        // Calls int version
        System.out.println(math.add(5.5, 10.2));    // Calls double version
        System.out.println(math.add(5, 10, 15));    // Calls three-parameter version
    }
}
```

### **Method Overriding**
```java
class Animal {
    void makeSound() {
        System.out.println("Animal makes a sound");
    }
}

class Dog extends Animal {
    @Override
    void makeSound() {
        System.out.println("Dog barks");
    }
}

class Test {
    public static void main(String[] args) {
        Animal myAnimal = new Dog(); // Upcasting
        myAnimal.makeSound(); // Calls overridden method in Dog class -> Output: "Dog barks"
    }
}
```

---

## **Static vs Non-Static Methods in Java**

In Java, methods can be categorized as **static** or **non-static** (instance methods). Understanding their differences is crucial for writing efficient and well-structured code.

### **1. Definition**
- **Static Methods**: Methods that belong to the class and can be called without creating an object.
- **Non-Static Methods**: Methods that belong to an instance (object) of the class and require an object to be invoked.

### **2. Key Differences**

| Feature               | Static Methods                                      | Non-Static Methods                                |
|-----------------------|----------------------------------------------------|--------------------------------------------------|
| **Belongs To**        | Class                                             | Instance (object)                               |
| **Invocation**        | Called using `ClassName.methodName()` or directly | Called using `objectName.methodName()`         |
| **Access to Instance Variables** | Cannot access instance variables directly | Can access all instance variables and methods |
| **Access to Static Variables** | Can access static variables and other static methods | Can access static variables and methods |
| **Overriding**        | Cannot be overridden (method hiding applies)       | Can be overridden in subclasses                |
| **Use Case**          | Utility methods, helper functions                  | Behavior specific to object state              |
| **Memory Allocation** | Loaded once in memory (Class area)                 | Created separately for each instance           |


### **3. Example Code**

### **Static Method Example**
```java
class MathUtils {
    static int square(int x) {
        return x * x;
    }
}

class Test {
    public static void main(String[] args) {
        System.out.println(MathUtils.square(5)); // Output: 25
    }
}
```

### **Non-Static Method Example**
```java
class Person {
    String name;

    Person(String name) {
        this.name = name;
    }

    void greet() {
        System.out.println("Hello, my name is " + name);
    }
}

class Test {
    public static void main(String[] args) {
        Person person = new Person("Mihai");
        person.greet(); // Output: Hello, my name is Mihai
    }
}
```

---

## **Java Streams**

Streams in Java provide a declarative approach to processing collections of data. They enable functional-style operations on elements.

### **1. Key Characteristics**
- **Pipeline Operations**: Chain multiple operations together
- **Lazy Evaluation**: Operations are only performed when needed
- **No Storage**: Streams don't store elements
- **Functional in Nature**: Operations don't modify the source

### **2. Common Operations**

| Operation Type | Examples | Description |
|---------------|----------|-------------|
| **Intermediate** | `filter()`, `map()`, `sorted()` | Return a stream, can be chained |
| **Terminal** | `collect()`, `forEach()`, `reduce()` | Produce a result or side-effect |

### **3. Example Code**
```java
List<String> names = Arrays.asList("John", "Jane", "Bob", "Alice");

// Basic stream operations
names.stream()
    .filter(name -> name.startsWith("J"))
    .map(String::toUpperCase)
    .sorted()
    .forEach(System.out::println);

// Common collectors
List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
int sum = numbers.stream()
    .reduce(0, Integer::sum);
double average = numbers.stream()
    .mapToInt(Integer::intValue)
    .average()
    .orElse(0.0);
```

---

## **Atomic Variables in Java**

Atomic variables provide thread-safe operations without using explicit synchronization.

### **1. Key Features**
- **Thread-Safe**: Operations are atomic (all-or-nothing)
- **Lock-Free**: Better performance than synchronized blocks
- **Compare-and-Set**: Atomic update operations

### **2. Common Atomic Classes**
- `AtomicInteger`
- `AtomicLong`
- `AtomicBoolean`
- `AtomicReference<V>`

### **3. Example Code**
```java
class Counter {
    private AtomicInteger count = new AtomicInteger(0);

    public void increment() {
        count.incrementAndGet(); // Atomic operation
    }

    public int getValue() {
        return count.get();
    }

    // Compare and set example
    public void updateIfEquals(int expected, int newValue) {
        count.compareAndSet(expected, newValue);
    }
}
```

---

## **CountDownLatch in Java**

CountDownLatch is a synchronization aid that allows one or more threads to wait until a set of operations in other threads completes.

### **1. Key Features**
- **One-Time Use**: Cannot be reset after reaching zero
- **Thread Coordination**: Helps synchronize multiple threads
- **Countdown Mechanism**: Counts down from initial count to zero

### **2. Common Methods**
- `await()`: Wait for latch to reach zero
- `countDown()`: Decrease count by one
- `getCount()`: Get current count

### **3. Example Code**
```java
class Worker implements Runnable {
    private final CountDownLatch latch;

    Worker(CountDownLatch latch) {
        this.latch = latch;
    }

    public void run() {
        try {
            // Do work
            Thread.sleep(1000);
            latch.countDown();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}

// Usage
CountDownLatch latch = new CountDownLatch(3);
ExecutorService executor = Executors.newFixedThreadPool(3);

for (int i = 0; i < 3; i++) {
    executor.submit(new Worker(latch));
}

latch.await(); // Wait for all workers to finish
System.out.println("All workers completed");
```

---

## **Semaphore in Java**

A Semaphore controls access to a shared resource through a counter representing permits.

### **1. Key Features**
- **Permit System**: Controls access using permits
- **Fair/Unfair**: Can be configured for fairness
- **Flexible**: Can increase/decrease permits dynamically

### **2. Common Methods**
- `acquire()`: Get a permit
- `release()`: Return a permit
- `availablePermits()`: Check available permits

### **3. Example Code**
```java
class BoundedResource {
    private final Semaphore semaphore;

    public BoundedResource(int permits) {
        semaphore = new Semaphore(permits);
    }

    public void useResource() throws InterruptedException {
        try {
            semaphore.acquire();
            // Use the resource
            Thread.sleep(1000);
        } finally {
            semaphore.release();
        }
    }
}

// Usage
BoundedResource resource = new BoundedResource(3);
ExecutorService executor = Executors.newFixedThreadPool(5);

for (int i = 0; i < 5; i++) {
    executor.submit(() -> {
        try {
            resource.useResource();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    });
}
```
