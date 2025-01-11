package com.example.collections.stack;

import java.util.Stack;
import java.util.stream.Stream;

public interface MyIStack<T> {
    void push(T v);
    T pop();
    boolean isEmpty();
    String toString();
    Stack<T> deepCopy();
    Stream<T> stream();
}
