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
}