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
    public PrgState execute(PrgState state) {
        return new PrgState(stmt, state.getSymTable().deepCopy(), state.getOut(), state.getFileTable(), state.getHeap());
    }
    
    @Override
    public IStmt deepCopy() {
        return new ForkStmt(stmt.deepCopy());
    }
}
