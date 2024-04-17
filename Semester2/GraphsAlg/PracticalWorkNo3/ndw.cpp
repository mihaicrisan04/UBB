#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <queue>
#include <stack>

using namespace std;


void ndw(vector<vector<int>>& g, int n, int a, int b) {
    vector<int> count(n+1, 0), indegree(n+1, 0);
    queue<int> q;

    // topological sort
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
            // *remove the edge from the graph, marking it as computed
            if (--indegree[neigh] == 0)  // if the indegree of the node is 0, that means that all the previous nodes have been computed
                q.push(neigh);

            count[neigh] += count[curr];
        }  
    }

    cout << "The number of walks from node " << a << " to node " << b << " is " << count[b] << '\n';
}

int main() {
    ifstream fin ("ndw.in");
    vector<vector<int>> g;
    int n, m, a, b;

    fin >> n >> m >> a >> b;
    g = vector<vector<int>>(n+1);
    while (m--) {
        int x, y;
        fin >> x >> y;
        g[x].push_back(y);
    }

    ndw(g, n, a, b);

    fin.close();
    return 0;
}