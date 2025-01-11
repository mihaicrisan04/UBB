package com.example.model;

import java.io.BufferedReader;

import com.example.model.exceptions.MyException;
import com.example.collections.heap.MyIHeap;
import com.example.collections.list.*;
import com.example.collections.stack.*;
import com.example.collections.dictionary.*;
import com.example.collections.heap.*;
import com.example.model.statements.IStmt;
import com.example.model.values.StringValue;
import com.example.model.values.Value;


public class PrgState {
   private MyIStack<IStmt> exeStack;
   private MyIDictionary<String, Value> symTable;
   private MyIList<Value> out;
   private IStmt originalProgram;
   private MyIDictionary<StringValue, BufferedReader> fileTable;
   private MyIHeap<Integer, Value> heap;
   private int id;

   private static int prgId = 0;

   public PrgState(IStmt prg, MyIDictionary<String, Value> symtbl, MyIList<Value> ot, MyIDictionary<StringValue, BufferedReader> fileTbl, MyIHeap<Integer, Value> hp) {
      exeStack = new MyStack<IStmt>();
      symTable = symtbl;
      out = ot;
      originalProgram = prg.deepCopy();
      fileTable = fileTbl;
      heap = hp;
      exeStack.push(prg);
      id = getNewId();
   }

   public PrgState(IStmt prg) {
      exeStack = new MyStack<IStmt>();
      symTable = new MyDictionary<String, Value>();
      out = new MyList<Value>();
      fileTable = new MyDictionary<StringValue, BufferedReader>();
      heap = new MyHeap<Integer, Value>();
      originalProgram = prg.deepCopy();
      exeStack.push(prg);
      id = getNewId();
   } 
   
   public MyIStack<IStmt> getExeStack() { return exeStack; }
   public MyIDictionary<String, Value> getSymTable() { return symTable; }
   public MyIList<Value> getOut() { return out; } // get out :))
   public MyIDictionary<StringValue, BufferedReader> getFileTable() { return fileTable; }
   public MyIHeap<Integer, Value> getHeap() { return heap; }
   public IStmt getOriginalProgram() { return originalProgram; }
   public int getId() { return id; }

   public void setExeStack(MyIStack<IStmt> stk) { exeStack = stk; }
   public void setSymTable(MyIDictionary<String, Value> symtbl) { symTable = symtbl; }
   public void setOut(MyIList<Value> ot) { out = ot; }
   public void setFileTable(MyIDictionary<StringValue, BufferedReader> fileTbl) { fileTable = fileTbl; }
   public void setHeap(MyIHeap<Integer, Value> hp) { heap = hp; }
   public void setOriginalProgram(IStmt prg) { originalProgram = prg; }

   public boolean isNotCompleted() { return !exeStack.isEmpty(); }   
   public static synchronized int getNewId() { return prgId++; }


   public PrgState oneStep() throws MyException {
      if (exeStack.isEmpty()) { throw new MyException("Program state stack is empty"); }
      IStmt crtStmt = exeStack.pop();
      return crtStmt.execute(this);
   }

   @Override
   public String toString() {
      return "------------------------------------------------------\n" +
             "[ID: " + id + "]\n" +
             "ExeStack:\n" + exeStack.toString() + "\n" +
             "SymTable:\n" + symTable.toString() + "\n" +
             "FileTable:\n" + fileTable.toString() + "\n" +
             "HeapTable:\n" + heap.toString() + "\n" +
             "Out:\n" + out.toString() + "\n" +
             "Original Program:\n" + originalProgram.toString() + "\n";
   }
}