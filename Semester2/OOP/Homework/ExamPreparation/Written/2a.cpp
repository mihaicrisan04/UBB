#include <iostream>

using namespace std;

class A {
private:
    int* x;
public:
    static int noOfOccurences;

    A(int _x = 0) {
        x = new int(_x);
        noOfOccurences++;
    }
    int get() { return *x; }
    void set(int _x) { *x = _x; }
    ~A() { delete x; }
};

int A::noOfOccurences = 0;

int main() {
    A a1, a2;
    cout << a1.noOfOccurences << endl;
    A a3 = a1;
    cout << A::noOfOccurences << endl;
    a1.set(8);
    cout << a1.get() << " ";
    cout << a2.get() << " ";
    cout << a3.get() << " ";
    return 0;
}
