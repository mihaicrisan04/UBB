package repository;

import model.Tree;

public interface IRepository {
    void add(Tree tree) throws CapacityExceededException;
    void remove(Tree tree) throws Exception;
    Tree[] all();
    int size();
}

