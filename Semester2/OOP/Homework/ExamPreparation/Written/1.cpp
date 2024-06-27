#include <iostream>
#include <vector>
#include <algorithm>
#include <stdexcept>

using namespace std;

template<typename T>
class SmartPointer {
private:
    T* ptr;
public:
    SmartPointer(T* p = nullptr) : ptr(p) {}
    ~SmartPointer() { delete ptr; }
    SmartPointer& operator=(const SmartPointer& other) {
        if (this != &other) {
            delete ptr;
            ptr = new T(*other.ptr);
        }
        return *this;
    }
    bool operator==(const SmartPointer& other) const {
        return *ptr == *other.ptr;
    }
    T& operator*() { return *ptr; }
    T* operator->() { return ptr; }
};

template<typename T>
class Vector {
private:
    std::vector<T> elements;
public:
    Vector& add(const T& element) {
        elements.push_back(element);
        return *this;
    }
    Vector operator-(const T& element) {
        for (auto it = elements.begin(); it != elements.end(); ++it) {
            if (*it == *element) {
                elements.erase(it);
                return *this;
            }
        }
        throw std::runtime_error("Element not found");
    }
    Vector& operator=(const Vector& other) {
        elements = other.elements;
        return *this;
    }
    typename std::vector<T>::iterator begin() { return elements.begin(); }
    typename std::vector<T>::iterator end() { return elements.end(); }
};

void function1() {
    SmartPointer<int> i1{ new int{ 1 } };
    SmartPointer<int> i2{ new int{ 2 } };
    SmartPointer<int> i3{ new int{ 3 } };
    Vector<SmartPointer<int>> v1{};
    v1.add(i1).add(i2).add(i3);
    for (auto e : v1)
        cout << *e << ", "; // prints 1, 2, 3
    SmartPointer<string> s1(new string("A") );
    SmartPointer<string> s2 = s1;
    SmartPointer<string> s3( new string("C") );
    Vector<SmartPointer<string> > v2;
    v2.add(s2).add(s1); 
    try { 
        v2 = v2 - s2;
        v2 = v2 - s3;
    }
    catch(std::runtime_error& e) {
        cout << e.what() << endl; // prints "Element not found"
    }
}

int main() {
    function1();
    return 0;
}
