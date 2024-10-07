package repository;

import model.Item;

public class Repository implements IRepository{

    private Item[] items;
    private static int capacity=2;
    private int size=0;

    public Repository(){
        items =new Item[capacity];
        size=0;
    }

    public Item[] getItems() {
        return items;
    }

    public void add (Item fruit) throws CapacityExceededException{
        if(size<capacity)items[size++]=fruit;
        else throw new CapacityExceededException("max. capacity exceeded");
        //if(size>=capacity) resize();
        //items[size++]=fruit;
    }

    private void resize(){
        Item[] items1 = new Item[capacity*2];
        for (int index=0; index<capacity; index++){
            items1[index] = items[index];
        }
        capacity*=2;
        items=items1;
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