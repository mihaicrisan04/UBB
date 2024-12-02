package model.statements;

import model.PrgState;

public class ForkStmt implements IStmt {
    private IStmt stmt;

    public ForkStmt(IStmt stmt) {
        this.stmt = stmt;
    }

    public IStmt getStmt() {
        return stmt;
    }

    @Override
    public String toString() {
        return "fork(" + stmt + ")";
    }

    @Override
    public PrgState execute(PrgState prg) {
        return new PrgState(stmt, prg.getSymTable().deepCopy(), prg.getOut(), prg.getFileTable(), prg.getHeap());
    }
    
    @Override
    public IStmt deepCopy() {
        return new ForkStmt(stmt.deepCopy());
    }
}
