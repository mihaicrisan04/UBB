package com.example.controller;


import com.example.model.statements.*;
import com.example.model.statements.file.*;
import com.example.model.statements.heap.*;
import com.example.model.expressions.*;
import com.example.model.types.*;
import com.example.model.values.*;
import com.example.model.enums.*;

import java.util.List;

import com.example.collections.list.*;


public class Programs {
    private MyIList<IStmt> programs;

    public Programs() {
        programs = new MyList<IStmt>();

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

        // int v; Ref int a; v=10;new(a,22);
        // fork(wH(a,30);v=32;print(v);print(rH(a))); 
        // print(v);print(rH(a))
        IStmt ex10 = new CompoundStmt(
            new VarDeclStmt("v", new IntType()),
            new CompoundStmt(
                new VarDeclStmt("a", new RefType(new IntType())),
                new CompoundStmt(
                    new AssignStmt("v", new ValueExp(new IntValue(10))),
                    new CompoundStmt(
                        new NewHeapStmt("a", new ValueExp(new IntValue(22))),
                        new CompoundStmt(
                            new ForkStmt(
                                new CompoundStmt(
                                    new WriteHeapStmt("a", new ValueExp(new IntValue(30))),
                                    new CompoundStmt(
                                        new AssignStmt("v", new ValueExp(new IntValue(32))),
                                        new CompoundStmt(
                                            new PrintStmt(new VarExp("v")),
                                            new PrintStmt(new ReadHeapExp(new VarExp("a")))
                                        )
                                    )
                                )
                            ),
                            new CompoundStmt(
                                new PrintStmt(new VarExp("v")),
                                new PrintStmt(new ReadHeapExp(new VarExp("a")))
                            )
                        )
                    )
                )
            ) 
        );

        // stmt that will fail the type check
        // int v; v=true; print(v)
        IStmt ex11 = new CompoundStmt(
            new VarDeclStmt("v", new IntType()),
            new CompoundStmt(
                new AssignStmt("v", new ValueExp(new BoolValue(true))),
                new PrintStmt(new VarExp("v"))
            )
        );

        // int a; int b; int c;
        // a=1;b=2;c=5;
        // (switch(a*10)
        // (case (b*c) : print(a);print(b))
        // (case (10) : print(100);print(200))
        // (default : print(300)));
        // print(300)
        IStmt ex12 = new CompoundStmt(
            new VarDeclStmt("a", new IntType()),
            new CompoundStmt(
                new VarDeclStmt("b", new IntType()),
                new CompoundStmt(
                    new VarDeclStmt("c", new IntType()),
                    new CompoundStmt(
                        new AssignStmt("a", new ValueExp(new IntValue(1))),
                        new CompoundStmt(
                            new AssignStmt("b", new ValueExp(new IntValue(2))),
                            new CompoundStmt(
                                new AssignStmt("c", new ValueExp(new IntValue(5))),
                                new CompoundStmt(
                                    new SwitchStmt(
                                        new ArithExp(
                                            new VarExp("a"),
                                            new ValueExp(new IntValue(10)),
                                            ArithOperation.MUL
                                        ),
                                        new ArithExp(
                                            new VarExp("b"),
                                            new VarExp("c"),
                                            ArithOperation.MUL
                                        ),
                                        new CompoundStmt(
                                            new PrintStmt(new VarExp("a")),
                                            new PrintStmt(new VarExp("b"))
                                        ),
                                        new ValueExp(new IntValue(10)),
                                        new CompoundStmt(
                                            new PrintStmt(new ValueExp(new IntValue(100))),
                                            new PrintStmt(new ValueExp(new IntValue(200)))
                                        ),
                                        new PrintStmt(new ValueExp(new IntValue(300)))
                                    ),
                                    new PrintStmt(new ValueExp(new IntValue(300)))
                                )
                            )
                        )
                    )
                )
            )
        );

        // Ref int v1; int cnt;
        // new(v1,1);createSemaphore(cnt,rH(v1));
        // fork(acquire(cnt);wh(v1,rh(v1)*10));print(rh(v1));release(cnt));
        // fork(acquire(cnt);wh(v1,rh(v1)*10));wh(v1,rh(v1)*2));print(rh(v1));release(cnt
        // ));
        // acquire(cnt);
        // print(rh(v1)-1);
        // release(cnt)

        programs.add(ex);
        programs.add(ex2);
        programs.add(ex3);
        programs.add(ex4);
        programs.add(ex5);
        programs.add(ex6);
        programs.add(ex7);
        programs.add(ex8);
        programs.add(ex9);
        programs.add(ex10);
        programs.add(ex11);
        programs.add(ex12);

        // TextMenu menu = new TextMenu();
        // menu.addCommand(new ExitCommand("0", "exit"));
        // menu.addCommand(new RunExCommand("1", ex.toString(), ex, "logs/log1.txt"));
        // menu.addCommand(new RunExCommand("2", ex2.toString(), ex2, "logs/log2.txt"));
        // menu.addCommand(new RunExCommand("3", ex3.toString(), ex3, "logs/log3.txt"));
        // menu.addCommand(new RunExCommand("4", ex4.toString(), ex4, "logs/log4.txt"));
        // menu.addCommand(new RunExCommand("5", ex5.toString(), ex5, "logs/log5.txt"));
        // menu.addCommand(new RunExCommand("6", ex6.toString(), ex6, "logs/log6.txt"));
        // menu.addCommand(new RunExCommand("7", ex7.toString(), ex7, "logs/log7.txt"));
        // menu.addCommand(new RunExCommand("8", ex8.toString(), ex8, "logs/log8.txt"));
        // menu.addCommand(new RunExCommand("9", ex9.toString(), ex9, "logs/log9.txt"));
        // menu.addCommand(new RunExCommand("10", ex10.toString(), ex10, "logs/log10.txt"));
        // menu.addCommand(new RunExCommand("11", ex11.toString(), ex11, "logs/log11.txt"));
        // menu.show();
    }

    public MyIList<IStmt> getPrograms() { return programs; }

    public List<String> getProgramStrings() {
        List<String> programStrings = new java.util.ArrayList<String>();
        for (IStmt program : programs) {
            programStrings.add(program.toString());
        }
        return programStrings;
    }

    public IStmt getProgramFromString(String programString) {
        for (IStmt program : programs) {
            if (program.toString().equals(programString)) {
                return program;
            }
        }
        return null;
    }

    public String getProgramLogFile(String programString) {
        for (int i = 0; i < programs.size(); i++) {
            if (programs.get(i).toString().equals(programString)) {
                return "logs/log" + (i + 1) + ".txt";
            }
        }
        return null;
    }
}
