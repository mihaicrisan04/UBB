package com.example.model.exceptions;

public class MyException extends Exception {
    public MyException(String message) {
        super(message);
    }
    public MyException() {
        super("MyException");
    }
}
