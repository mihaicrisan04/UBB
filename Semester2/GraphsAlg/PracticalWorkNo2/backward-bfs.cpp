#include <iostream>
#include <fstream>
#include <vector>
#include <queue>

using namespace std;


void backward_bfs(vector<vector<int>> &g, int a, int b) {
    vector<int> dist(g.size(), -1);
    vector<int> visited(g.size(), 0);
    vector<int> parent(g.size(), -1);
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
                parent[neigh] = node;
                q.push(neigh);
            }
        }
    }

    if (dist[b] == -1) {
        cout << "No path from " << a << " to " << b << "\n";
    } 
    else {
        cout << "Distance from " << a << " to " << b << " is " << dist[b] << "\n";
        cout << "Path: ";
        while (b != -1) {
            cout << b << " ";
            b = parent[b];
        }
        cout << "\n";
    }
}



int main() {
    ifstream fin ("bfs-data.in");
    vector<vector<int>> gt;
    int n, m, a, b;
    
    fin >> n >> m >> a >> b;
    gt = vector<vector<int>>(n+1);
    for (int i = 0; i < m; i++) {
        int x, y;
        fin >> x >> y;
        gt[y].push_back(x);
    }

    backward_bfs(gt, b, a);

    fin.close();
    return 0;
}