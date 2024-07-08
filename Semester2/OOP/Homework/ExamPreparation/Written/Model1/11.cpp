#include <iostream>
#include <vector>
#include <stdexcept>
#include <string>

template <typename T>
class SmartPointer {
private:
    T* p;

public: 
    SmartPointer(T* p) : p(p) {}

    SmartPointer& operator=(SmartPointer& other) {
        if (*this != other) {
            p = other.p;
        }
        return *this;
    }

    bool operator==(SmartPointer& other) {
        return p == other.p;
    }

    bool operator!=(SmartPointer& other) {
        return !(*this == other);
    }

    T& operator*() { return *p; }
};

template <typename T>
class Vector {
private:
    std::vector<T> v;
public:
    Vector& add(T value) {
        v.push_back(value);
        return *this;
    }

    typename std::vector<T>::iterator begin() { return v.begin(); } 
    typename std::vector<T>::iterator end() { return v.end(); }

    bool operator==(Vector& other) {
        return v != other.v;
    }

    bool operator!=(Vector& other) {
        return !(*this == other);
    }

    Vector& operator=(Vector& other) {
        if (*this != other) {
            v = other.v;
        }
        return *this;
    }

    Vector& operator-(T value) {
        auto it = std::find(v.begin(), v.end(), value);
        if (it != v.end()) {
            v.erase(it);
        }
        else throw std::runtime_error("Element not found");
        return *this;
    }
};


int main() {
    SmartPointer<int> i1{new int{1}};
    SmartPointer<int> i2{new int{2}};
    SmartPointer<int> i3{new int{3}};
    Vector<SmartPointer<int>> v1{};
    v1.add(i1).add(i2).add(i3);
    for (auto e: v1) {
        std::cout << *e << std::endl;
    }
    SmartPointer<std::string> s1{new std::string{ "a"}};
    SmartPointer<std::string> s2 = s1;
    SmartPointer<std::string> s3{new std::string{ "c"}};
    Vector<SmartPointer<std::string>> v2{};
    v2.add(s2).add(s1);
    for (auto e: v2) {
        std::cout << *e << std::endl;
    }
    try {
        v2 = v2 - s2;
        v2 = v2 - s3;
    }
    catch(std::runtime_error& ex) {
        std::cout << ex.what() << std::endl;
    }
    return 0;
}