package repository;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.IOException;

import collections.list.MyIList;
import collections.list.MyList;
import model.PrgState;
import model.exceptions.MyException;

public class Repository implements IRepository {
    private MyIList<PrgState> programList;
    private String logFilePath;

    public Repository() {
        programList = new MyList<PrgState>();
        logFilePath = "";
    }
    
    public Repository(MyIList<PrgState> list) {
        programList = list;
        logFilePath = "";
    }

    public Repository(MyIList<PrgState> list, String logFilePath) {
        programList = list;
        this.logFilePath = logFilePath;
    }

    @Override
    public PrgState getCurrentProgram() { return programList.get(0); }

    @Override
    public MyIList<PrgState> getProgramList() { return programList; }

    @Override
    public void setProgramList(MyIList<PrgState> list) { programList = list; }

    @Override
    public void logPrgStateExec() throws IOException {
        System.out.println(getCurrentProgram().toString()); // log to console
        PrintWriter logFile = new PrintWriter(new BufferedWriter(new FileWriter(logFilePath, true)));
        logFile.write(getCurrentProgram().toString());
        logFile.close();
    }

    public void addProgram(PrgState prg) {
        programList.add(prg);
    }

    public void removeProgram(int index) {
        programList.remove(index);
    }

    public void removeProgram(PrgState prg) {
        for (int i = 0; i < programList.size(); i++) {
            if (programList.get(i).equals(prg)) {
                programList.remove(i);
                break;
            }
        }
    }

    public void clear() {
        programList.clear();
    }

    public int size() {
        return programList.size();
    }

    public PrgState getProgram(int index) {
        return programList.get(index);
    }

    public void clearCompleted() {
        for (int i = 0; i < programList.size(); i++) {
            if (!programList.get(i).isNotCompleted()) {
                programList.remove(i);
                i--;
            }
        }
    }
}