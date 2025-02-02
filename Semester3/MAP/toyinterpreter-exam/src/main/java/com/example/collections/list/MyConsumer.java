package com.example.collections.list;

@FunctionalInterface
public interface MyConsumer<T> {
    void accept(T t);
}