package view;

import model.PrgState;
import model.exceptions.MyException;
import model.statements.IStmt;
import repository.Repository;
import controller.Controller;

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
            PrgState prgState = new PrgState(prg);
            Repository repo = new Repository(logFilePath);
            repo.addProgram(prgState);
            Controller ctrl = new Controller(repo);
            ctrl.executeAllSteps();
        } catch(MyException e) {
            System.out.println(e.getMessage());
        }
    }
}