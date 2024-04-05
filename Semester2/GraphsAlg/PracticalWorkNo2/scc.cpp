#include <iostream>
#include <fstream>
#include <vector>
#include <stack>

using namespace std;

void dfs(int node, vector<vector<int>> &g, vector<bool> &visited, stack<int> &st) {
    visited[node] = true;
    for (auto neigh: g[node]) {
        if (!visited[neigh]) {
            dfs(neigh, g, visited, st);
        }
    }
    st.push(node);
}

void dfs2(int node, vector<vector<int>> &g, vector<bool> &visited) {
    visited[node] = true;
    cout << node << " ";
    for (auto neigh : g[node]) {
        if (!visited[neigh]) {
            dfs2(neigh, g, visited);
        }
    }
}

// Complexity O(n + m)
void kosaraju(int n, vector<vector<int>> &g, vector<vector<int>> &gt) {
    stack<int> st;
    vector<bool> visited(n + 1, false);

    // First DFS to get the order of the nodes
    for (int i = 1; i <= n; i++) {
        if (!visited[i]) {
            dfs(i, g, visited, st);
        }
    }

    // Reset visited
    for (int i = 1; i <= n; i++) {
        visited[i] = false;
    }

    // go through the stack and get the SCCs
    while (!st.empty()) {
        int v = st.top();
        st.pop();

        // DFS on the transpose to get an SCC
        if (!visited[v]) {
            dfs2(v, gt, visited);
            cout << "\n";
        }
    }
}

int main() {
    ifstream fin ("scc-data.in");
    vector<vector<int>> g;
    vector<vector<int>> gt;
    int n, m;

    fin >> n >> m;
    g = vector<vector<int>>(n+1);
    gt = vector<vector<int>>(n+1);

    for (int i = 0; i < m; i++) {
        int x, y;
        fin >> x >> y;
        g[x].push_back(y);
        gt[y].push_back(x);
    }

    kosaraju(n, g, gt);

    return 0;
}