package collections.list;

public interface MyIList<T> {
    int size();
    boolean isEmpty();
    boolean add(T elem);
    void clear();
    T get(int index);
    String toString();
}
