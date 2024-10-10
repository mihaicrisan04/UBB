package repository;

import model.Tree;

public class Repository implements IRepository {
    private Tree[] trees;
    private static int capacity = 10;
    private int size;

    public Repository() {
        trees = new Tree[capacity];
        size = 0;
    }

    private void resize() {
        Tree[] newTrees = new Tree[capacity * 2];
        for (int i = 0; i < size; i++) {
            newTrees[i] = trees[i];
        }
        trees = newTrees;
        capacity *= 2;
    }

    @Override
    public void add(Tree tree) throws CapacityExceededException {
        if (size == capacity) {
            throw new CapacityExceededException();
        }
        trees[size++] = tree;
    }

    @Override
    public void remove(Tree tree) throws Exception {
        int index = -1;
        for (int i = 0; i < size; i++) {
            if (trees[i].equals(tree)) {
                index = i;
                break;
            }
        }
        if (index == -1) {
            throw new Exception("Tree not found");
        }
        for (int i = index; i < size - 1; i++) {
            trees[i] = trees[i + 1];
        }
        size--;
    }

    @Override
    public Tree[] all() {
        Tree[] allTrees = new Tree[size];
        System.arraycopy(trees, 0, allTrees, 0, size);
        return allTrees;
    }

    @Override
    public int size() {
        return size;
    }

}
