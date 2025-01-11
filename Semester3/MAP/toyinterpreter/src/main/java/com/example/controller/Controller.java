package com.example.controller;

import com.example.collections.heap.MyIHeap;
import com.example.collections.heap.MyHeap;
import com.example.collections.list.MyIList;
import com.example.collections.list.MyList;
import com.example.repository.IRepository;
import com.example.model.exceptions.MyException;
import com.example.model.PrgState;
import com.example.model.values.Value;
import com.example.model.values.RefValue;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;


public class Controller {
    IRepository repo;
    public ExecutorService executor;

    public Controller() {
        repo = null;
    }

    public Controller(IRepository r) {
        repo = r;
    }

    public int getNumberOfPrograms() { return repo.getProgramList().size(); }
    public MyIList<PrgState> getProgramList() { return repo.getProgramList(); }

    public MyIHeap<Integer, Value> conservativeGarbageCollector(MyIList<PrgState> prgList, MyIHeap<Integer, Value> heap) {
        // Collect all reachable addresses from all program states
        Set<Integer> reachableAddresses = new HashSet<>();
        for (PrgState prg : prgList) {
            reachableAddresses.addAll(getAddrFromSymTable(prg.getSymTable().values()));
        }
    
        // Collect all reachable addresses from the heap
        boolean added;
        do {
            added = false;
            Set<Integer> newAddresses = heap.entrySet().stream()
                    .filter(e -> reachableAddresses.contains(e.getKey()))
                    .map(e -> e.getValue())
                    .filter(v -> v instanceof RefValue)
                    .map(v -> ((RefValue) v).getAddress())
                    .filter(addr -> !reachableAddresses.contains(addr))
                    .collect(Collectors.toSet());
            added = reachableAddresses.addAll(newAddresses);
        } while (added);
    
        // Filter and collect the entries into a new map
        Map<Integer, Value> newHeapContent = heap.entrySet().stream()
                .filter(e -> reachableAddresses.contains(e.getKey()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    
        // Create a new instance of MyHeap with the filtered content
        MyIHeap<Integer, Value> newHeap = new MyHeap<>();
        newHeap.setContent(newHeapContent);
        return newHeap;
    } 

    public MyIList<Integer> getAddrFromSymTable(Collection<Value> symTableValues) {
        return symTableValues.stream()
                .filter(v -> v instanceof RefValue)
                .map(v -> ((RefValue) v).getAddress())
                .collect(Collectors.toCollection(MyList::new));
    }

    public void oneStepForAllPrg(MyIList<PrgState> prgList) throws MyException {
        for (PrgState prg : prgList) { repo.logPrgStateExec(prg); }

        MyIList<Callable<PrgState>> callList = prgList.stream()
                .map((PrgState p) -> (Callable<PrgState>)(() -> { return p.oneStep(); }))
                .collect(Collectors.toCollection(MyList::new));

        MyIList<PrgState> newPrgList;
        try {
            newPrgList = executor.invokeAll(callList).stream()
                .map(future -> {
                    try {
                        return future.get();
                    } catch (Exception e) {
                        return null;
                    }
                })
                .filter(p -> p != null)
                .collect(Collectors.toCollection(MyList::new));
        } catch (InterruptedException e) {
            throw new MyException(e.getMessage());
        }
        
        prgList.addAll(newPrgList);
        for (PrgState prg : prgList) { repo.logPrgStateExec(prg); }
        repo.setProgramList(prgList);
    }

    public void executeAllSteps() throws MyException {
        executor = java.util.concurrent.Executors.newFixedThreadPool(2);
        MyIList<PrgState> prgList = removeCompletedPrg(repo.getProgramList());
        while (prgList.size() > 0) {
            synchronized (repo) {
                // Garbage collector
                MyIHeap<Integer, Value> heap = repo.getProgramList().get(0).getHeap();
                heap = conservativeGarbageCollector(prgList, heap);
                for (PrgState prg: prgList) { prg.setHeap(heap); }
            }
            oneStepForAllPrg(prgList);
            prgList = removeCompletedPrg(repo.getProgramList());
        }
        executor.shutdownNow();
        repo.setProgramList(prgList);
    }

    public MyIList<PrgState> removeCompletedPrg(MyIList<PrgState> inPrgList) {
        return inPrgList.stream()
                .filter(p -> p.isNotCompleted())
                .collect(Collectors.toCollection(MyList::new));
    }

    public IRepository getRepo() { return repo; }
    public void setRepo(IRepository r) { repo = r; }
}
