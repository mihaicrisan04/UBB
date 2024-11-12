package collections.stack;

import java.util.Stack;

public class MyStack<T> implements MyIStack<T> {    
    private Stack<T> stack;

    public MyStack() {
        stack = new Stack<T>();
    }

    @Override
    public void push(T v) {
        stack.push(v);
    }

    @Override
    public T pop() {
        return stack.pop();
    }

    @Override
    public boolean isEmpty() {
        return stack.isEmpty();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (T elem : stack) {
            sb.insert(0, elem.toString()).insert(0, "\n");
        }
        if (sb.length() == 0) return "";

        // remove the last newline char from the front 
        sb.deleteCharAt(0);
        
        return sb.toString();
    }

    @Override
    public Stack<T> deepCopy() {
        Stack<T> newStack = new Stack<T>();
        for (T elem : stack) {
            newStack.push(elem);
        }
        return newStack;
    }
}
