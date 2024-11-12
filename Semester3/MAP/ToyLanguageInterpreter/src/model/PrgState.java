package model;

import model.exceptions.MyException;
import collections.dictionary.*;
import collections.stack.*;
import collections.list.*;
import model.statements.IStmt;
import model.values.Value;


public class PrgState {
   private MyIStack<IStmt> exeStack;
   private MyIDictionary<String, Value> symTable;
   private MyIList<Value> out;
   private IStmt originalProgram;
   // private TreeNode exeTreeRoot;
   // private MyIDictionary<String, Value> fileTable;

   public PrgState(MyIStack<IStmt> stk, MyIDictionary<String, Value> symtbl, MyIList<Value> ot, IStmt prg) {
      exeStack = stk;
      symTable = symtbl;
      out = ot;
      originalProgram = prg.deepCopy();
      exeStack.push(prg);
   }

   public PrgState(IStmt prg) {
      exeStack = new MyStack<IStmt>();
      symTable = new MyDictionary<String, Value>();
      out = new MyList<Value>();
      originalProgram = prg.deepCopy();
      exeStack.push(prg);
   } 
   
   public MyIStack<IStmt> getExeStack() { return exeStack; }

   public MyIDictionary<String, Value> getSymTable() { return symTable; }

   public MyIList<Value> getOut() { return out; } // get out :))

   public IStmt getOriginalProgram() { return originalProgram; }

   public void setExeStack(MyIStack<IStmt> stk) { exeStack = stk; }

   public void setSymTable(MyIDictionary<String, Value> symtbl) { symTable = symtbl; }

   public void setOut(MyIList<Value> ot) { out = ot; }

   public void setOriginalProgram(IStmt prg) { originalProgram = prg; }

   public boolean isNotCompleted() { return !exeStack.isEmpty(); }   

   // TODO: implement heap
   // public MyIDictionary<Integer, Integer> getHeap() { return heap; }

   public PrgState oneStep() throws MyException {
      if (exeStack.isEmpty()) { throw new MyException("Program state stack is empty"); }
      IStmt crtStmt = exeStack.pop();
      return crtStmt.execute(this);
   }

   @Override
   public String toString() {
      return "------------------------------------------------------\n" +
             "ExeStack:\n" + exeStack.toString() + "\n" +
             "SymTable:\n" + symTable.toString() + "\n" +
             "Out:\n" + out.toString() + "\n" +
             "Original Program:\n" + originalProgram.toString() + "\n";
   }
}