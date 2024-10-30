
import collections.list.*;
import model.expressions.*;
import model.statements.*;
import model.types.*;
import model.values.*;
import model.PrgState;
import model.enums.*;
import repository.IRepository;
import repository.Repository;
import controller.Controller;
import view.Console;


public class App {
    public static void main(String[] args) throws Exception {
        IStmt ex = new CompoundStmt(
            new VarDeclStmt("v", new IntType()),
            new CompoundStmt(
                new AssignStmt("v", new ValueExp(new IntValue(2))),
                new PrintStmt(new VarExp("v"))
            )
        );

        IStmt ex2 = new CompoundStmt(
            new VarDeclStmt("v", new IntType()),
            new CompoundStmt(
                new AssignStmt("v", new ArithExp(
                    new ValueExp(new IntValue(2)),
                    new ArithExp(
                        new ValueExp(new IntValue(3)),
                        new ValueExp(new IntValue(5)),
                        ArithOperation.ADD
                    ),
                    ArithOperation.MUL
                )),
                new PrintStmt(new VarExp("v"))
            )
        );

        IStmt ex3 = new CompoundStmt(
            new VarDeclStmt("a", new IntType()),
            new CompoundStmt(
                new VarDeclStmt("b", new IntType()),
                new CompoundStmt(
                    new AssignStmt("a", new ArithExp(
                        new ValueExp(new IntValue(2)),
                        new ArithExp(
                            new ValueExp(new IntValue(3)),
                            new ValueExp(new IntValue(5)),
                            ArithOperation.ADD
                        ),
                        ArithOperation.MUL
                    )),
                    new CompoundStmt(
                        new AssignStmt("b", new ArithExp(
                            new VarExp("a"),
                            new ValueExp(new IntValue(1)),
                            ArithOperation.ADD
                        )),
                        new PrintStmt(new VarExp("b"))
                    )
                )
            )
        );

        IStmt ex4 = new CompoundStmt(
            new VarDeclStmt("a", new BoolType()),
            new CompoundStmt(
                new VarDeclStmt("v", new IntType()),
                new CompoundStmt(
                    new AssignStmt("a", new ValueExp(new BoolValue(true))),
                    new CompoundStmt(
                        new IfStmt(
                            new VarExp("a"),
                            new AssignStmt("v", new ValueExp(new IntValue(2))),
                            new AssignStmt("v", new ValueExp(new IntValue(3)))
                        ),
                        new PrintStmt(new VarExp("v"))
                    )
                )
            )
        );

        MyIList<PrgState> prgList = new MyList<PrgState>();
        prgList.add(new PrgState(ex));
        prgList.add(new PrgState(ex2));
        prgList.add(new PrgState(ex3));
        prgList.add(new PrgState(ex4));

        IRepository repo = new Repository(prgList);
        Controller ctrl = new Controller(repo);
        Console console = new Console(ctrl);
        console.run();
    }
}
