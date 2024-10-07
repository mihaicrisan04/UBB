package repository;

import model.Item;

public class Repository implements IRepository {
    private Item[] items;
    private static int capacity;
    private int size = 0;

    public Repository() {
        items = new Item[capacity];
        size = 0;
    }

    public Item[] getItems() {
        return items;
    }

    @Override
    public void add(Item fruit) throws CapacityExceededException {
        if (size == capacity) {
            throw new CapacityExceededException();
            // resize?
        }
        items[size++] = fruit;
    }

    private void resize() {
        Item[] temp = new Item[capacity * 2];
        for (int i = 0; i < capacity; i++) {
            temp[i] = items[i];
        }
        items = temp;
        capacity *= 2;
    }

    @Override
    public Item[] all() {
        return items;
    }

    @Override
    public int size() {
        return size;
    }

}
