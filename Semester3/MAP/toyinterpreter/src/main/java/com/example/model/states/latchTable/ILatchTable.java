package com.example.model.states.latchTable;

import java.util.HashMap;

import com.example.model.exceptions.MyException;

public interface ILatchTable {
    public void put(int key, Integer value) throws MyException;
    Integer get(int key) throws MyException;    
    boolean containsKey(int key);
    int getFreeAddress();
    void setFreeAddress(int freeAddress);
    void update(int key, Integer value) throws MyException;
    HashMap<Integer, Integer> getLatchTable();
    void setLatchTable(HashMap<Integer, Integer> newLatchTable);
    String toString();
}

