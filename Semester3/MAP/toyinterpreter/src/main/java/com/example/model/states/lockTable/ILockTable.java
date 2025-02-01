package com.example.model.states.lockTable;

import java.util.HashMap;

import com.example.model.exceptions.MyException;

public interface ILockTable {
    public void put(int key, Integer value) throws MyException;
    Integer get(int key) throws MyException;    
    boolean containsKey(int key);
    int getFreeAddress();
    void setFreeAddress(int freeAddress);
    void update(int key, Integer value) throws MyException;
    HashMap<Integer, Integer> getLockTable();
    void setLockTable(HashMap<Integer, Integer> newLockTable);
    String toString();
}
