/*
keyword override forgotten


Output:
B.f Function h B.f B.f B.f 

Actual Output:
B.f Function h D.f D.f B.f B.f
*/

#include <iostream>


class B {
private:
    virtual void f() {
        std::cout << "B.f ";
    }
public:
    virtual ~B() {
        f();
    }
    void g() {
        f();
    }
    virtual void h() {
        g();
    }
};

class D : public B {
private:
    void f() {
        std::cout << "D.f ";
    }
public:
    void h() {
        std::cout << "Function h ";
        B::g();
    }
    ~D() {
        g();
    }
};

int main() {
    B b{};
    D d{};
    B& dd = d;
    b.g(); // B.f
    dd.h(); // Function h, B::g() -> D::f() D.f
            // skip the refrence of d
            // ~D() -> B::g() -> D::f() D.f -> ~B() -> B::f() B.f
            // ~B() -> B::f() B.f
    return 0;
}