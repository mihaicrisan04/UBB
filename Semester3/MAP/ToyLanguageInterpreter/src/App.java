
import model.expressions.*;
import model.statements.*;
import model.statements.file.*;
import model.statements.heap.*;
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
                    new OpenFileStmt(new VarExp("varf")),
                    new CompoundStmt(
                        new VarDeclStmt("varc", new IntType()),
                        new CompoundStmt(
                            new ReadFileStmt(new VarExp("varf"), "varc"),
                            new CompoundStmt(
                                new PrintStmt(new VarExp("varc")),
                                new CompoundStmt(
                                    new ReadFileStmt(new VarExp("varf"), "varc"),
                                    new CompoundStmt(
                                        new PrintStmt(new VarExp("varc")),
                                        new CloseFileStmt(new VarExp("varf"))
                                    )
                                )
                            )
                        )
                    )
                )
            )
        );

        // Ref int v;new(v,20);Ref Ref int a; new(a,v);print(v);print(a)
        IStmt ex6 = new CompoundStmt(
            new VarDeclStmt("v", new RefType(new IntType())),
            new CompoundStmt(
                new NewHeapStmt("v", new ValueExp(new IntValue(20))),
                new CompoundStmt(
                    new VarDeclStmt("a", new RefType(new RefType(new IntType()))),
                    new CompoundStmt(
                        new NewHeapStmt("a", new VarExp("v")),
                        new CompoundStmt(
                            new PrintStmt(new VarExp("v")),
                            new PrintStmt(new VarExp("a"))
                        )
                    )
                )
            )
        );

        // Ref int v; new(v,20); print(rH(v)); wH(v,30); print(rH(v)+5);
        IStmt ex7 = new CompoundStmt(
            new VarDeclStmt("v", new RefType(new IntType())),
            new CompoundStmt(
                new NewHeapStmt("v", new ValueExp(new IntValue(20))),
                new CompoundStmt(
                    new PrintStmt(new ReadHeapExp(new VarExp("v"))),
                    new CompoundStmt(
                        new WriteHeapStmt("v", new ValueExp(new IntValue(30))),
                        new PrintStmt(new ArithExp(
                            new ReadHeapExp(new VarExp("v")),
                            new ValueExp(new IntValue(5)),
                            ArithOperation.ADD
                        ))
                    )
                )
            )
        );

        // int v; v=4; (while (v>0) print(v);v=v-1);print(v)
        IStmt ex8 = new CompoundStmt(
            new VarDeclStmt("v", new IntType()),
            new CompoundStmt(
                new AssignStmt("v", new ValueExp(new IntValue(4))),
                new CompoundStmt(
                    new WhileStmt(
                        new CompareExp(
                            new VarExp("v"),
                            new ValueExp(new IntValue(0)),
                            CompareOperation.GREATER
                        ),
                        new CompoundStmt(
                            new PrintStmt(new VarExp("v")),
                            new AssignStmt("v", new ArithExp(
                                new VarExp("v"),
                                new ValueExp(new IntValue(1)),
                                ArithOperation.SUB
                            ))
                        )
                    ),
                    new PrintStmt(new VarExp("v"))
                )
            )
        );

        // Ref int v;new(v,20);Ref Ref int a; new(a,v); new(v,30);print(rH(rH(a)))
        IStmt ex9 = new CompoundStmt(
            new VarDeclStmt("v", new RefType(new IntType())),
            new CompoundStmt(
                new NewHeapStmt("v", new ValueExp(new IntValue(20))),
                new CompoundStmt(
                    new VarDeclStmt("a", new RefType(new RefType(new IntType()))),
                    new CompoundStmt(
                        new NewHeapStmt("a", new VarExp("v")),
                        new CompoundStmt(
                            new NewHeapStmt("v", new ValueExp(new IntValue(30))),
                            new PrintStmt(new ReadHeapExp(new ReadHeapExp(new VarExp("a"))))
                        )
                    )
                )
            )
        );


        TextMenu menu = new TextMenu();
        menu.addCommand(new ExitCommand("0", "exit"));
        menu.addCommand(new RunExCommand("1", ex.toString(), ex, "log1.txt"));
        menu.addCommand(new RunExCommand("2", ex2.toString(), ex2, "log2.txt"));
        menu.addCommand(new RunExCommand("3", ex3.toString(), ex3, "log3.txt"));
        menu.addCommand(new RunExCommand("4", ex4.toString(), ex4, "log4.txt"));
        menu.addCommand(new RunExCommand("5", ex5.toString(), ex5, "log5.txt"));
        menu.addCommand(new RunExCommand("6", ex6.toString(), ex6, "log6.txt"));
        menu.addCommand(new RunExCommand("7", ex7.toString(), ex7, "log7.txt"));
        menu.addCommand(new RunExCommand("8", ex8.toString(), ex8, "log8.txt"));
        menu.addCommand(new RunExCommand("9", ex9.toString(), ex9, "log9.txt"));
        menu.show();
    }
}
