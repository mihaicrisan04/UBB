package collections.dictionary;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public interface MyIDictionary<K, V> {
    V get(K key);
    V put(K key, V value);
    int size();
    String toString();
    boolean containsKey(K key);
    void remove(K key);
    Collection<V> values();
    boolean containsValue(V value);
    K getKey(V value);
    Set<K> keySet();
    // Set<Map.Entry<K, V>> entrySet();
    // void setContent(Map<K, V> map);
    // MyIDictionary<K, V> clone();
}
