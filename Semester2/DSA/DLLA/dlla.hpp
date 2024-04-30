#pragma once

template <typename T>
class DLLA {
private:
    struct Node {
        T value;
        int next;
        int prev;
    }        

    Node *arr;
    int capacity;
    int size;
    int head;
    int tail;
    int firstEmpty;

    void resize() {

    }

public:
    DLLA(int capacity = 100) : capacity(capacity), size(0), head(-1), tail(-1), firstEmpty(0) {
        arr = new Node[capacity];
        for (int i = 0; i < capacity; i++) {
            arr[i].next = i + 1;
            arr[i].prev = i - 1;
        }
        arr[capacity - 1].next = -1;
    }

    void add(T value) {

    }


    void remove(T value) {

    }

    ~DLLA() {
        delete[] arr;
    }

};