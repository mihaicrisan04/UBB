package com.example.model.states.latchTable;

import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;

import com.example.model.exceptions.MyException;


public class LatchTable implements ILatchTable {
    private HashMap<Integer, Integer> latchTable;
    ReentrantLock lock;
    private int freeLocation = 0;

    public LatchTable() {
        this.latchTable = new HashMap<>();
        this.lock = new ReentrantLock();
    }

    @Override
    public synchronized void put(int key, Integer value) throws MyException {
        if (latchTable.containsKey(key)) { throw new MyException("Latch table already contains the key!"); }
        latchTable.put(key, value);
    }

    @Override
    public synchronized Integer get(int key) throws MyException {
        if (!latchTable.containsKey(key)) { throw new MyException(String.format("Latch table doesn't contain the key %d!", key)); }
        return latchTable.get(key);
    }

    @Override
    public synchronized boolean containsKey(int key) {
        return latchTable.containsKey(key);
    }

    @Override
    public synchronized int getFreeAddress() {
        freeLocation++;
        return freeLocation;
    }

    @Override
    public synchronized void setFreeAddress(int freeAddress) {
        this.freeLocation = freeAddress;
    }

    @Override
    public synchronized void update(int key, Integer value) throws MyException {
        if (!latchTable.containsKey(key)) { throw new MyException(String.format("Latch table doesn't contain the key %d!", key)); }
        latchTable.put(key, value);
    }

    @Override
    public synchronized HashMap<Integer, Integer> getLatchTable() {
        return latchTable;
    }

    @Override
    public synchronized void setLatchTable(HashMap<Integer, Integer> newLatchTable) {
        latchTable = newLatchTable;
    }

    @Override
    public synchronized String toString() {
        StringBuilder sb = new StringBuilder();
        for (var entry : latchTable.entrySet()) {
            sb.append(entry.getKey()).append(" -> ").append(entry.getValue()).append("\n");
        }
        return sb.toString();
    }
    
}
