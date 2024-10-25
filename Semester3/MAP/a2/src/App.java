
import model.exceptions.*;
import model.expressions.*;
import model.statements.*;
import model.types.*;
import model.values.*;
import model.PrgState;
import model.enums.*;
import collections.*;



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

        PrgState prg = new PrgState(ex2);
        System.out.println(prg);
        while (prg.isNotCompleted()) {
            prg.oneStep();
            System.out.println(prg);
        }
    }
}
