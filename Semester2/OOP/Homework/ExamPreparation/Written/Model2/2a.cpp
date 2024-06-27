/*
error no operator = implemented
A& operator= (A& other) {
    this->a = other.a;
    return *this;
}
C& operator= (C& other) {
    this->a = other.a;
    return *this;
}

C::get() will not return an int
because it return *this

c1 = a2 = 2
c2 = 2
c3 = 4

1 2 2 2 4
1 2 4 2 4 // correct answer

*/
#include <iostream>

using namespace std;

class A {
private:
    int x;
public:
    A(int _x = 1) : x(_x) {}
    int get() const { return x; }
    friend class C;
};

class C {
private:
    A a;
public:
    C(const A& _a) : a(_a) {}
    C& operator*(const A& _a) {
        a.x *= _a.x;
        return *this;
    }
    int get() { return a.get(); }
};

int main() {
    A a1{}, a2{2};
    cout << a1.get() << ' ' << a2.get() << ' ';
    C c1{ a2 };
    C c2 = c1 * a1;
    C c3 = c1 * a2;
    cout << c1.get() << ' ' << c2.get() << ' ' << c3.get();
    return 0;
}