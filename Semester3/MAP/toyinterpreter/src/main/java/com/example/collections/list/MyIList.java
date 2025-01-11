package com.example.collections.list;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

public interface MyIList<T> extends Collection<T> {
    int size();
    boolean isEmpty();
    boolean add(T elem);
    T remove(int index);
    void clear();
    T get(int index);
    String toString();
    void forEach(MyConsumer<T> consumer);
    Iterator<T> iterator();
    Stream<T> stream();
    void addAll(MyIList<T> list);
    List<T> toList();
}
