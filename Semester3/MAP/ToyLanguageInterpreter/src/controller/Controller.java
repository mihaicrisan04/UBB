package controller;

import collections.heap.MyIHeap;
import collections.heap.MyHeap;
import collections.list.MyIList;
import collections.list.MyList;
import repository.IRepository;
import model.exceptions.MyException;
import model.PrgState;
import model.values.Value;
import model.values.RefValue;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Callable;
import java.util.List;
import java.util.stream.Collectors;


public class Controller {
    IRepository repo;
    private ExecutorService executor;

    public Controller() {
        repo = null;
    }

    public Controller(IRepository r) {
        repo = r;
    }

    public int getNumberOfPrograms() {
        return repo.getProgramList().size();
    }

    public MyIList<PrgState> getProgramList() {
        return repo.getProgramList();
    }

    private MyIHeap<Integer, Value> safeGarbageCollector(List<Integer> symTableAddr, MyIHeap<Integer, Value> heap) {
        // Collect all reachable addresses
        Set<Integer> reachableAddresses = new HashSet<>(symTableAddr);
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

    private List<Integer> getAddrFromSymTable(Collection<Value> symTableValues) {
        return symTableValues.stream()
                .filter(v -> v instanceof RefValue)
                .map(v -> ((RefValue) v).getAddress())
                .toList();
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
                        System.out.println(e.getMessage());
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
            // conservative garbage collector call
            oneStepForAllPrg(prgList);
            prgList = removeCompletedPrg(repo.getProgramList());
        }
        executor.shutdownNow();
        repo.setProgramList(prgList);
    }

    private MyIList<PrgState> removeCompletedPrg(MyIList<PrgState> inPrgList) {
        return inPrgList.stream()
                .filter(p -> p.isNotCompleted())
                .collect(Collectors.toCollection(MyList::new));
    }
}
