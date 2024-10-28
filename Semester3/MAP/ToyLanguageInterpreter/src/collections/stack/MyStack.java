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
            sb.append(elem.toString()).append("\n");
        }
        return sb.toString();
    }
}
