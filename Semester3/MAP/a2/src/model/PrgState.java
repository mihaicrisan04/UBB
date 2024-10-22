package model;

import model.exceptions.MyException;
import collections.dictionary.*;
import collections.stack.*;
import collections.list.*;
import model.statements.IStmt;


public class PrgState {
   private MyIStack<IStmt> exeStack;
   private MyIDictionary<String, Integer> symTable;
   private MyIList<Integer> out;
   private IStmt originalProgram;

   PrgState(MyIStack<IStmt> stk, MyIDictionary<String, Integer> symtbl, MyIList<Integer> ot, IStmt prg) {
      exeStack = stk;
      symTable = symtbl;
      out = ot;
      originalProgram = prg; // TODO: deep copy?
      stk.push(prg);
   }
   
   MyIStack<IStmt> getExeStack() { return exeStack; }

   MyIDictionary<String, Integer> getSymTable() { return symTable; }

   MyIList<Integer> getOut() { return out; } // get out :))

   IStmt getOriginalProgram() { return originalProgram; }

   public void setExeStack(MyIStack<IStmt> stk) { exeStack = stk; }

   public void setSymTable(MyIDictionary<String, Integer> symtbl) { symTable = symtbl; }

   public void setOut(MyIList<Integer> ot) { out = ot; }

   public void setOriginalProgram(IStmt prg) { originalProgram = prg; }

   public boolean isNotCompleted() { return !exeStack.isEmpty(); }   

   public PrgState oneStep() throws MyException {
      if (exeStack.isEmpty()) {
         throw new MyException("Program state stack is empty");
      }
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