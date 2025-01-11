package com.example.view.command;

import com.example.collections.dictionary.MyDictionary;
import com.example.model.PrgState;
import com.example.model.exceptions.MyException;
import com.example.model.statements.IStmt;
import com.example.repository.Repository;
import com.example.controller.Controller;

public class RunExCommand extends Command {
    private IStmt prg;
    private String logFilePath;

    public RunExCommand(String key, String description, IStmt prg) {
        this(key, description, prg, "");
    }

    public RunExCommand(String key, String description, IStmt prg, String logFilePath) {
        super(key, description);
        this.prg = prg;
        this.logFilePath = logFilePath;
    }

    @Override
    public void execute() {
        try {
            prg.typeCheck(new MyDictionary<>());
            PrgState prgState = new PrgState(prg);
            Repository repo = new Repository(logFilePath);
            repo.addProgram(prgState);
            Controller ctrl = new Controller(repo);
        ctrl.executeAllSteps();
        } catch (MyException e) {
            System.out.println(e.getMessage());
        } 
    }
}
