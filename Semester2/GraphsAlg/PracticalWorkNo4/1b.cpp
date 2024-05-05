#include <iostream>
#include <fstream>
#include <vector>
#include <string.h>

using namespace std;

struct Node {
    int data;
    Node *left;
    Node *right;
};


Node* createNode(int data) {
    Node *newNode = new Node();
    newNode->data = data;
    newNode->left = NULL;
    newNode->right = NULL;

    return newNode;
}

Node* buildTreePre(vector<int> &inorder, vector<int> &preorder, int inStart, int inEnd) {
    if (inStart > inEnd) {
        return NULL;
    }
    
    int rootData = preorder[0];
    Node* root = createNode(rootData);
    preorder.erase(preorder.begin());

    int k = 0;
    for (int i = inStart; i<= inEnd; i++) {
        if (inorder[i] == rootData) {
            k = i;
            break;
        }
    }

    root->left = buildTreePre(inorder, preorder, inStart, k - 1);
    root->right = buildTreePre(inorder, preorder, k + 1, inEnd);

    return root;
}

Node* buildTreePost(vector<int> &inorder, vector<int> &postorder, int inStart, int inEnd) {
    if (inStart > inEnd) {
        return NULL;
    }

    int rootData = postorder.back();
    Node* root = createNode(rootData);
    postorder.pop_back();

    int k = 0;
    for (int i = inStart; i <= inEnd; i++) {
        if (inorder[i] == rootData) {
            k = i;
            break;
        }
    }

    root->right = buildTreePost(inorder, postorder, k + 1, inEnd);
    root->left = buildTreePost(inorder, postorder, inStart, k - 1);

    return root;
}

void printInorder(Node *root) {
    if (root == NULL) {
        return;
    }

    printInorder(root->left);
    cout << root->data << " ";
    printInorder(root->right);
}

int main() {
    ifstream fin ("input-1b.txt");
    char s[100];
    vector<vector<int>> t(3);

    for (int i = 0; i < 3; i++) {
        fin.getline(s, 100);

        char* p = strtok(s, " ");
        while (p != NULL) {
            int num = atoi(p);
            t[i].push_back(num);
            p = strtok(NULL, " ");
        }
    }

    vector<int> inorder = t[0];
    vector<int> postorder = t[1]; 
    vector<int> preorder = t[2];

    Node* rootPre = buildTreePre(inorder, preorder, 0, inorder.size() - 1);
    Node* rootPost = buildTreePost(inorder, postorder, 0, inorder.size() - 1);

    printInorder(rootPre);
    cout << endl;
    printInorder(rootPost);

    fin.close();
    return 0;
}