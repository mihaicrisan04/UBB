package com.example.collections.list;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

import java.util.Collection;

public class MyList<T> implements MyIList<T> {
    private List<T> list;

    public MyList() {
        list = new ArrayList<>();
    }

    public MyList(List<T> list) {
        this.list = list;
    }

    @Override
    public boolean contains(Object o) {
        return list.contains(o);
    }

    @Override
    public Object[] toArray() {
        return list.toArray();
    }

    @Override
    public <U> U[] toArray(U[] a) {
        return list.toArray(a);
    }

    @Override
    public boolean remove(Object o) {
        return list.remove(o);
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        return list.containsAll(c);
    }

    @Override
    public boolean addAll(Collection<? extends T> c) {
        return list.addAll(c);
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        return list.removeAll(c);
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        return list.retainAll(c);
    }

    @Override
    public boolean add(T elem) {
        return list.add(elem);
    }

    @Override
    public T remove(int index) {
        return list.remove(index);
    }

    @Override
    public void clear() {
        list.clear();
    }

    @Override
    public T get(int index) {
        return list.get(index);
    }

    @Override
    public boolean isEmpty() {
        return list.isEmpty();
    }

    @Override
    public int size() {
        return list.size();
    }

    @Override
    public String toString() {
        return list.toString();
    }

    @Override
    public Iterator<T> iterator() {
        return list.iterator();
    }

    @Override
    public void forEach(MyConsumer<T> consumer) {
        for (T item : list) {
            consumer.accept(item);
        }
    }

    @Override
    public Stream<T> stream() {
        return list.stream();
    }

    @Override
    public void addAll(MyIList<T> list) {
        for (T item : list) {
            this.add(item);
        }
    }

    @Override
    public List<T> toList() {
        return list;
    }

}
