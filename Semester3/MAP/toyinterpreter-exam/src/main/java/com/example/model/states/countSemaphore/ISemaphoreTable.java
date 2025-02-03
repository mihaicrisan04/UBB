package com.example.model.states.countSemaphore;

import java.util.HashMap;
import java.util.List;

import javafx.util.Pair;

import com.example.model.exceptions.MyException;

public interface ISemaphoreTable {
        void put(int key, Pair<Integer, List<Integer>> value) throws MyException;
        Pair<Integer, List<Integer>> get(int key) throws MyException;
        boolean containsKey(int key);
        int getFreeAddress();
        void setFreeAddress(int freeAddress);
        void update(int key, Pair<Integer, List<Integer>> value) throws MyException;
        HashMap<Integer, Pair<Integer, List<Integer>>> getSemaphoreTable();
        List<Pair<Pair<Integer, Integer>, List<Integer>>> getSemaphoreDictionaryAsList();
        void setSemaphoreTable(HashMap<Integer, Pair<Integer, List<Integer>>> newSemaphoreTable);
        String toString();
}
