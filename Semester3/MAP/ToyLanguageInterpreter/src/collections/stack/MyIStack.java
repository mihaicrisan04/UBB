package collections.stack;

import java.util.Stack;

public interface MyIStack<T> {
    void push(T v);
    T pop();
    boolean isEmpty();
    String toString();
    Stack<T> deepCopy();
}
