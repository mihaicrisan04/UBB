#include <iostream>

using namespace std;

class B {
public:
    B() {}
    B(const B&) { cout << "copy "; }
    virtual B f() { cout << "B.f"; return *this; }
    virtual ~B() { cout << "~B "; }
};

class D : public B {
private:
    B* b;
public:
    D(B* _b) : b{_b} { cout << "D "; }
    B f() override { cout << "D.f "; return b->f(); }
};

int main() {
    B* b = new B();
    B* d = new D(b); // D 
    d->f(); // D.f B.fcopy ~B 
    delete d; // ~B 
    delete b; // ~B
    return 0;
    //D D.f B.fcopy ~B ~B ~B 
}
