#include <iostream>
#include <vector>
#include <stack>
#include <string>

using namespace std;

template <typename T>
class InfixExpression {
private:    
    T value;
    stack<T> values; 
public:
    InfixExpression(T value) {
        this->value = value;
    }

    InfixExpression& operator= (InfixExpression& other) {
        this->value = other.value;
        this->values = other.values;
        return *this;
    }

    InfixExpression& operator- (int rhs) {
        values.push(value);
        this->value -= rhs;
        return *this;
    }

    InfixExpression& operator+ (int rhs) {
        values.push(value);
        this->value += rhs;
        return *this;
    }

    InfixExpression& undo() {
        if (!values.empty()) {
            this->value = values.top();
            values.pop();
        }
        else {
            throw runtime_error("Cannot undo");
        }
        return *this;
    }

    int getValue() {
        return this->value;
    }
};

int main() {
    InfixExpression<int> exp(2);
    exp = exp + 5 + 10;
    exp = exp - 8;
    cout << exp.getValue() << endl;    
    exp.undo();
    cout << exp.getValue() << endl;
    exp.undo().undo();
    try {
        exp.undo();
    }
    catch (runtime_error& e) {
        cout << e.what() << endl;
    }
    return 0;
}