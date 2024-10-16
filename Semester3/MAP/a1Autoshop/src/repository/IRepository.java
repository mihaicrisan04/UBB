package repository;

import model.Vehicle;

public interface IRepository {
    void add(Vehicle vehicle) throws RepositoryException;
    Vehicle get(int index) throws RepositoryException;
    void remove(int index) throws RepositoryException;
    Vehicle[] getAll();
    int size();
}
