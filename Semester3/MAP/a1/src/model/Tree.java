package model;

public abstract class Tree {
    protected String name;
    protected int age;

    public Tree(String name, int age) {
        this.name = name;
        this.age = age;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        Tree tree = (Tree) obj;
        return name.equals(tree.name) && age == tree.age;
    }

    public String toString() {
        return "Name: " + name + ", Age: " + age;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public String getName() {
        return name;
    }

    public int getAge() {
        return age;
    }
}
