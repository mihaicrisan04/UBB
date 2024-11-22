package collections.heap;

import java.util.Map;
import java.util.Set;


public interface MyIHeap<K, V> {    
    public V get(K key);
    public V put(K key, V value);
    public K getNextFreeAddress();
    public int size();
    public String toString();
    public boolean containsKey(K key);
    public void remove(K key);
    public Map<K, V> getContent();
    public void setContent(Map<K, V> map);
    public MyIHeap<K, V> clone();
    public Set<Map.Entry<K, V>> entrySet();
}

