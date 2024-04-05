#include <iostream>
#include <fstream>
#include <vector>
#include <queue>

using namespace std;


int backward_bfs(vector<vector<int>> &g, int a, int b) {
    vector<int> dist(g.size(), -1);
    vector<int> visited(g.size(), 0);
    queue<int> q;

    q.push(a);
    dist[a] = 0;
    visited[a] = 1;

    while (!q.empty()) {
        int node = q.front();
        q.pop();
        for (auto &neigh : g[node]) {
            if (!visited[neigh]) {
                visited[neigh] = 1;
                dist[neigh] = dist[node] + 1;
                q.push(neigh);
            }
        }
    }
    return dist[b];
}



int main() {
    ifstream fin ("bfs-data.in");
    vector<vector<int>> g;
    int n, m, a, b;
    
    fin >> n >> m >> a >> b;
    g = vector<vector<int>>(n+1);
    for (int i = 0; i < m; i++) {
        int x, y;
        fin >> x >> y;
        g[x].push_back(y);
    }

    cout << backward_bfs(g, a, b) << "\n";

    fin.close();
    return 0;
}