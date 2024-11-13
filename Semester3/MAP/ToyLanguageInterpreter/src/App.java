
import model.expressions.*;
import model.statements.*;
import model.statements.files.*;
import model.types.*;
import model.values.*;
import model.enums.*;
import view.*;


public class App {
    public static void main(String[] args) {
        // int v; v=2; Print(v)
        IStmt ex = new CompoundStmt(
            new VarDeclStmt("v", new IntType()),
            new CompoundStmt(
                new AssignStmt("v", new ValueExp(new IntValue(2))),
                new PrintStmt(new VarExp("v"))
            )
        );

        // int a; int b; a=2+3; b=a+1; Print(b)
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

        // int a; int b; a=2+3*5; b=a+1; Print(b)
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

        // bool a; int v; a=true; (if a then v=2 else v=3); print(v)
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

        // string varf; varf="test.in"; openRFile(varf); int varc; readFile(varf,varc);print(varc); readFile(varf,varc);print(varc) closeRFile(varf)
        IStmt ex5 = new CompoundStmt(
            new VarDeclStmt("varf", new StringType()),
            new CompoundStmt(
                new AssignStmt("varf", new ValueExp(new StringValue("test.in"))),
                new CompoundStmt(
                    new OpenStmt(new VarExp("varf")),
                    new CompoundStmt(
                        new VarDeclStmt("varc", new IntType()),
                        new CompoundStmt(
                            new ReadStmt(new VarExp("varf"), "varc"),
                            new CompoundStmt(
                                new PrintStmt(new VarExp("varc")),
                                new CompoundStmt(
                                    new ReadStmt(new VarExp("varf"), "varc"),
                                    new CompoundStmt(
                                        new PrintStmt(new VarExp("varc")),
                                        new CloseStmt(new VarExp("varf"))
                                    )
                                )
                            )
                        )
                    )
                )
            )
        );

        // MyIList<PrgState> prgList = new MyList<PrgState>();
        // prgList.add(new PrgState(ex));
        // prgList.add(new PrgState(ex2));
        // prgList.add(new PrgState(ex3));
        // prgList.add(new PrgState(ex4));

        // IRepository repo = new Repository(prgList);
        // Controller ctrl = new Controller(repo);
        // Console console = new Console(ctrl, true);
        // console.run();

        TextMenu menu = new TextMenu();
        menu.addCommand(new ExitCommand("0", "exit"));
        menu.addCommand(new RunExCommand("1", ex.toString(), ex));
        menu.addCommand(new RunExCommand("2", ex2.toString(), ex2));
        menu.addCommand(new RunExCommand("3", ex3.toString(), ex3));
        menu.addCommand(new RunExCommand("4", ex4.toString(), ex4));
        menu.addCommand(new RunExCommand("5", ex5.toString(), ex5));
        menu.show();
    }
}
