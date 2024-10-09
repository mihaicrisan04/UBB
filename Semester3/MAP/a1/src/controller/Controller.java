package controller;

import model.Tree;
import repository.IRepository;
import repository.CapacityExceededException;

public class Controller {
    private IRepository repo;

    public Controller(IRepository repo) {
        this.repo = repo;
    }

    public Tree[] all() {
        return repo.all();
    }

    public int size() {
        return repo.size();
    }

    public void add(Tree tree) {
        try {
            repo.add(tree);
        } catch (CapacityExceededException e) {
            System.out.println(e.getMessage());
        }
    }

    public void remove(Tree tree) {
        try {
            repo.remove(tree);
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }

    public Tree[] filter(int age) {
        Tree[] result = new Tree[repo.size()];
        int size = 0;
        for (Tree tree: repo.all()) {
            if (tree.getAge() > age) {
                result[size++] = tree;
            }
        }
        Tree[] filtered = new Tree[size];
        System.arraycopy(result, 0, filtered, 0, size);
        return filtered;
    }
}
