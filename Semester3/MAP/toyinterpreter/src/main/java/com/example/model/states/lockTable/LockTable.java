package com.example.model.states.lockTable;

import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;

import com.example.model.exceptions.MyException;

public class LockTable implements ILockTable {
    private HashMap<Integer, Integer> lockTable;
    ReentrantLock lock;
    private int freeLocation = 0;

    public LockTable() {
        this.lockTable = new HashMap<>();
        this.lock = new ReentrantLock();
    }

    @Override
    public synchronized void put(int key, Integer value) throws MyException {
        if (lockTable.containsKey(key)) { throw new MyException("Lock table already contains the key!"); }
        lockTable.put(key, value);
    }

    @Override
    public synchronized Integer get(int key) throws MyException {
        if (!lockTable.containsKey(key)) { throw new MyException(String.format("Lock table doesn't contain the key %d!", key)); }
        return lockTable.get(key);
    }

    @Override
    public synchronized boolean containsKey(int key) {
        return lockTable.containsKey(key);
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
        if (!lockTable.containsKey(key)) { throw new MyException(String.format("Lock table doesn't contain the key %d!", key)); }
        lockTable.put(key, value);
    }

    @Override
    public synchronized HashMap<Integer, Integer> getLockTable() {
        return lockTable;
    }

    @Override
    public synchronized void setLockTable(HashMap<Integer, Integer> newLockTable) {
        lockTable = newLockTable;
    }

    @Override
    public synchronized String toString() {
        StringBuilder sb = new StringBuilder();
        lockTable.forEach((key, value) -> sb.append(String.format("%d -> %d\n", key, value)));
        return sb.toString();
    }
}
