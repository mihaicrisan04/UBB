package collections.list;

public interface MyIList<T> {
    int size();
    boolean isEmpty();
    boolean add(T elem);
    T remove(int index);
    void clear();
    T get(int index);
    String toString();
}
