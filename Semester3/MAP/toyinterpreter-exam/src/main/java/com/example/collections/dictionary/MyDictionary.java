package com.example.collections.dictionary;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.HashMap;

public class MyDictionary<K, V> implements MyIDictionary<K, V> {
    private Map<K, V> dict;

    public MyDictionary() {
        dict = new HashMap<>();
    }
    
    @Override
    public V get(K key) {
        return dict.get(key);
    }

    @Override
    public V put(K key, V value) {
        return dict.put(key, value);
    }

    @Override
    public int size() {
        return dict.size();
    }

    @Override
    public String toString() {
        return dict.toString();
    }

    @Override
    public boolean containsKey(K key) {
        return dict.containsKey(key);
    }

    @Override
    public void remove(K key) {
        dict.remove(key);
    }

    @Override
    public Collection<V> values() {
        return dict.values();
    }

    @Override
    public boolean containsValue(V value) {
        return dict.containsValue(value);
    }

    @Override
    public Set<K> keySet() {
        return dict.keySet();
    }

    @Override
    public K getKey(V value) {
        for (K key : dict.keySet()) {
            if (dict.get(key).equals(value)) {
                return key;
            }
        }
        return null;
    }

    @Override
    public Set<Map.Entry<K, V>> entrySet() {
        return dict.entrySet();
    }

    @Override
    public void setContent(Map<K, V> map) {
        dict = map;
    }

    @Override
    public MyIDictionary<K, V> deepCopy() {
        MyIDictionary<K, V> newDict = new MyDictionary<>();
        newDict.setContent(new HashMap<>(dict));
        return newDict;
    }
}
