#include <iostream>
#include <fstream>
#include <vector>
#include <stack>
#include <queue>
#include <cctype>
#include <string>

using namespace std;


bool isCyclicUtil(vector<vector<int>> &g, int node, vector<bool> &visited, vector<bool> &recStack) {
    visited[node] = true;
    recStack[node] = true;

    for (auto &neigh: g[node]) {
        if (!visited[neigh] && isCyclicUtil(g, neigh, visited, recStack)) {
            return true;
        } 
        else if (recStack[neigh]) {
            return true;
        }
    }

    recStack[node] = false;
    return false;
}

bool isDAG(vector<vector<int>> &g, int n) {
    vector<bool> visited(n+1, false);
    vector<bool> recStack(n+1, false);

    for (int i = 1; i <= n; i++) {
        if (!visited[i]) {
            if (isCyclicUtil(g, i, visited, recStack)) {
                return false;
            }
        }
    } 
    
    return true;
}    

void dfs(vector<vector<int>> &g, int node, vector<bool> &visited, stack<int> &s) {
    visited[node] = true;

    for (auto &neigh: g[node]) {
        if (!visited[neigh]) {
            dfs(g, neigh, visited, s);
        }
    }

    s.push(node);
}

vector<int> topologicalSort(vector<vector<int>> &g, int n) {
    vector<bool> visited(n+1, false);
    stack<int> s; 

    for (int i = n; i >= 1; i--) {
        if (!visited[i]) {
            dfs(g, i, visited, s);
        }
    }

    vector<int> topOrder;
    while (!s.empty()) {
        topOrder.push_back(s.top());
        s.pop();
    }

    return topOrder;
}

void ndw(vector<vector<int>>& g, int n, int a, int b) {
    vector<int> count(n+1, 0), indegree(n+1, 0);
    queue<int> q;

    // compute the indegree of each node
    for(int i=1; i<=n; i++)
        for(const int& j:g[i])
            indegree[j]++;

    // push all nodes with indegree 0 to start the computation from them
    for(int i=1; i<=n; i++)
        if(indegree[i] == 0)
            q.push(i);

    count[a] = 1;

    while (!q.empty()) {
        int curr = q.front();
        q.pop();

        for (const int& neigh: g[curr]) {
            if (--indegree[neigh] == 0)  // if the indegree of the node is 0, that means that all the previous nodes have been computed
                q.push(neigh);

            count[neigh] += count[curr];
        }
    }

    cout << "The number of walks from node " << a << " to node " << b << " is " << count[b] << '\n';
}

int main() {
    ifstream fin ("input-2b.txt");
    vector<vector<int>> g;
    int n, m, a, b;

    fin >> n >> m >> a >> b;
    g = vector<vector<int>>(n+1);
    while (m--) {
        int x, y;
        fin >> x >> y;
        g[x].push_back(y);
    }

    if (!isDAG(g, n)) {
        cout << "Not a DAG\n";
        return 0;
    }

    cout << "Topological sort:\n";
    vector<int> topOrder = topologicalSort(g, n);
    for (int i: topOrder) {
        cout << i << '\n';
    } 

    ndw(g, n, a, b);

    fin.close();
    return 0;
}
    