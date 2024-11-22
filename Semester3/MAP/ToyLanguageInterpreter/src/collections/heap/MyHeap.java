package collections.heap;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class MyHeap<K, V> implements MyIHeap<K, V> {
    private Map<K, V> heap;
    private Integer nextFreeAddress;

    public MyHeap() {
        heap = new HashMap<>();
        nextFreeAddress = 0;
    }

    public K getNextFreeAddress() {
        nextFreeAddress++;
        return (K) nextFreeAddress;
    }

    @Override
    public V get(K key) {
        return heap.get(key);
    }

    @Override
    public V put(K key, V value) {
        return heap.put(key, value);
    }

    @Override
    public int size() {
        return heap.size();
    }

    @Override
    public String toString() {
        return heap.toString();
    }

    @Override
    public boolean containsKey(K key) {
        return heap.containsKey(key);
    }

    @Override
    public void remove(K key) {
        heap.remove(key);
    }

    @Override
    public Map<K, V> getContent() {
        return heap;
    }

    @Override
    public void setContent(Map<K, V> map) {
        heap = map;
    }

    @Override
    public MyIHeap<K, V> clone() {
        MyHeap<K, V> newHeap = new MyHeap<>();
        newHeap.setContent(new HashMap<>(heap));
        return newHeap;
    }

    @Override
    public Set<Map.Entry<K, V>> entrySet() {
        return heap.entrySet();
    }
}
