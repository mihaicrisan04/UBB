package controller;

import collections.heap.MyIHeap;
import collections.heap.MyHeap;
import collections.list.MyIList;
import repository.IRepository;
import model.exceptions.MyException;
import model.PrgState;
import model.values.Value;
import model.values.RefValue;

import java.util.Collection;
import java.util.Map;
import java.util.List;
import java.util.stream.Collectors;


public class Controller {
    IRepository repo;

    public Controller() {
        repo = null;
    }

    public Controller(IRepository r) {
        repo = r;
    }

    public void oneStepForAllPrg(MyIList<PrgState> prgList) throws MyException {
        // TODO: implement this method
    }

    public int getNumberOfPrograms() {
        return repo.getProgramList().size();
    }

    public MyIList<PrgState> getProgramList() {
        return repo.getProgramList();
    }

    private MyIHeap<Integer, Value> unsafeGarbageCollector(List<Integer> symTableAddr, MyIHeap<Integer, Value> heap) {
        // Filter and collect the entries into a new map
        Map<Integer, Value> newHeapContent = heap.entrySet().stream()
                .filter(e -> symTableAddr.contains(e.getKey()))
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

    public void executeAllSteps() throws MyException {
        try {
            repo.logPrgStateExec();
            while (repo.getCurrentProgram().isNotCompleted()) {
                repo.getCurrentProgram().oneStep();
                repo.logPrgStateExec();

                repo.getCurrentProgram().getHeapTable().setContent(unsafeGarbageCollector(
                    getAddrFromSymTable(repo.getCurrentProgram().getSymTable().values()),
                    repo.getCurrentProgram().getHeapTable()
                ).getContent()); // garbage collector
            }
        } catch (MyException e) {
            throw e;
        } 
    }

    public void executeAllSteps(int index, boolean displayFlag) throws MyException {
        PrgState prg = repo.getProgramList().get(index);
        try {
            if (displayFlag) { System.out.println(prg.toString()); }
            while (prg.isNotCompleted()) {
                prg.oneStep();
                if (displayFlag) { System.out.println(prg.toString()); }
            }
            if (!displayFlag) { System.out.println(prg.toString()); }
        } catch (MyException e) {
            throw e;
        } catch (Exception e) {
            throw new MyException(e.getMessage());
        }
    }
}
