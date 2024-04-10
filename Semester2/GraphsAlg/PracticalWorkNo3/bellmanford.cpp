#include <iostream>
#include <fstream>
#include <vector>
#include <queue>

using namespace std;

const int INF = 1e9;

ifstream fin ("bellmanford.in");

void bellmanford(vector<vector<pair<int, int>>>& g, int n, int a, int b) {
    vector<int> visited(n+1, 0);
    vector<int> dist(n+1, INF);
    queue<int> q;
    bool negative_cycle = false;

    dist[a] = 0;
    q.push(a);

    while (!q.empty() && !negative_cycle) {
        int node = q.front();
        q.pop();
        visited[node]++;

        if (visited[node] >= n) {
            negative_cycle = true;
            cout << "Negative cycle\n";
            return;
        }

        for (pair<int,int> j: g[node]) { 
            int neigh = j.first;
            int cost = j.second;

            if (dist[neigh] > dist[node] + cost) {
                dist[neigh] = dist[node] + cost;
                q.push(neigh);
            }
        }
    }

    if (!negative_cycle) {
        for (int i = 1; i <= n; i++) {
            cout << i << '\t';
        }
        cout << '\n';
        for (int i = 1; i <= n; i++) {
            cout << dist[i] << '\t';
        }
    }
}

int main() {
    int n, m, a, b;
    vector<vector<pair<int,int>>> g;

    fin >> n >> m >> a >> b;
    g = vector<vector<pair<int, int>>>(n+1);

    for (int i = 0; i < m; i++) {
        int x, y, c;
        fin >> x >> y >> c;
        g[x].push_back({y, c});
    }

    bellmanford(g, n, a, b);

    fin.close();
    return 0;
}