#include <iostream>
#include <stack>
#include <string>

using namespace std;

class Node {
public:
    char data;
    Node *left, *right;
    Node(char data) : data(data), left(nullptr), right(nullptr) {}
};

void inorder(Node *node) {
    if (node) {
        inorder(node->left);
        cout << node->data << " ";
        inorder(node->right);
    }
}

int main() {
    string s = "ABC*+D/";
    stack<Node*> st;

    for (char c : s) {
        if (isalpha(c)) {
            st.push(new Node(c));
        } 
        else {
            Node *t = new Node(c);
            t->right = st.top();
            st.pop();
            t->left = st.top();
            st.pop();
            st.push(t);
        }
    }

    inorder(st.top());

    return 0;
}
