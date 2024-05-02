#pragma once
#include <vector>

template <typename T>
class DLLA {
private:
    struct Node {
        T value;
        int next;
        int prev;
    };

    Node *arr;
    int capacity;
    int size;
    int head;
    int tail;
    int firstEmpty;

    void resize() {
        Node *newArr = new Node[capacity * 2];
        for (int i = 0; i < capacity; i++) {
            newArr[i] = arr[i];
        }
        for (int i = capacity; i < capacity * 2; i++) {
            newArr[i].next = i + 1;
            newArr[i].prev = i - 1;
        }
        newArr[capacity * 2 - 1].next = -1;
        delete[] arr;
        arr = newArr;
        firstEmpty = capacity;
        capacity *= 2;
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
        if (size == capacity) {
            resize();
        }
        int newElem = firstEmpty;
        firstEmpty = arr[firstEmpty].next;
        arr[newElem].value = value;
        arr[newElem].next = -1;
        arr[newElem].prev = tail;
        if (size == 0) {
            head = newElem;
        } else {
            arr[tail].next = newElem;
        }
        tail = newElem;
        size++;
    }


    bool remove(T value) {
        int current = head;
        while (current != -1) {
            if (arr[current].value == value) {
                if (current == head) {
                    head = arr[current].next;
                }
                if (current == tail) {
                    tail = arr[current].prev;
                }
                if (arr[current].prev != -1) {
                    arr[arr[current].prev].next = arr[current].next;
                }
                if (arr[current].next != -1) {
                    arr[arr[current].next].prev = arr[current].prev;
                }
                arr[current].next = firstEmpty;
                firstEmpty = current;
                size--;
                return true;
            }
            current = arr[current].next;
        }
        return false;
    }

    int getSize() {
        return size;
    }

    std::vector<T> toVector() {
        std::vector<T> result;
        int current = head;
        while (current != -1) {
            result.push_back(arr[current].value);
            current = arr[current].next;
        }
        return result;
    }

    ~DLLA() {
        delete[] arr;
    }
};