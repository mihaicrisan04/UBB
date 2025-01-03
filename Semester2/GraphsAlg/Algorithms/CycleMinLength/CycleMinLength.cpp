#include <bits/stdc++.h>
#include <fstream>
#include <iostream>

using namespace std;

typedef pair<int, int> pii;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<pii> vii;
typedef vector<vii> vvii;

#define INF 1e9

vi findShortestCycle(const vvi &graph, int s, int n) {
    vvi dist(n + 1, vi(n + 1, INF));
    vvi parent(n + 1, vi(n + 1, -1));

    for (int i = 1; i <= n; i++) {
        for (int j : graph[i]) {
            dist[i][j] = 1;
            parent[i][j] = i;
        }
    }
    
    for (int k = 1; k <= n; k++) {
        for (int u = 1; u <= n; u++) {
            for (int v = 1; v <= n; v++) {
                if (dist[u][v] > dist[u][k] + dist[k][v]) {
                    dist[u][v] = dist[u][k] + dist[k][v];
                    parent[u][v] = parent[k][v];
                }
            }
        }
    }

    int min_cycle_len = INF;
    vi cycle;
    for (int i = 1; i <= n; i++) {
        if (dist[i][i] < min_cycle_len) {
            min_cycle_len = dist[i][i];
            cycle.clear();
            int v = i;
            while (true) {
                cycle.push_back(v);
                v = parent[i][v];
                if (v == i) {
                    cycle.push_back(v);
                    break;
                }
            }
            reverse(cycle.begin(), cycle.end());
        }
    }

    return cycle;
}

int main() {
    ifstream fin("CycleMinLength.in");
    vvi g;
    int n, m, s;

    fin >> n >> m >> s;
    g = vvi(n + 1);

    while (m--) {
        int u, v;
        fin >> u >> v;
        g[u].push_back(v);
    }

    vi shortest_cycle = findShortestCycle(g, s, n);
    for (int vertex : shortest_cycle) {
        cout << vertex << " ";
    }

    fin.close();
    return 0;
}