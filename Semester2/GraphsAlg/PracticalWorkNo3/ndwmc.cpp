#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <queue>

using namespace std;

const int INF = 1e9;

ifstream fin ("ndwmc.in");

struct cmp {
    bool operator()(pair<int, int> a, pair<int, int> b) {
        return a.first > b.first;
    }
};

void dijkstra(vector<vector<pair<int,int>>>& g, int n, int a, int b) {
    vector<int> dist(n+1, INF);
    vector<int> visited(n+1, 0);
    priority_queue<pair<int, int>, vector<pair<int, int>>, cmp> h;

    dist[a] = 0;
    h.push({0, a});

    while (!h.empty()) {
        int curr = h.top().second;
        h.pop();

        visited[curr] = 1;

        for (const auto& i : g[curr]) {
            int neigh = i.first;
            int cost = i.second;

            if (dist[neigh] > dist[curr] + cost) {
                dist[neigh] = dist[curr] + cost;
                if (!visited[neigh]) {
                    visited[neigh] = 1;
                    h.push({dist[neigh], neigh});
                }
            }
        }
    }

    for (int i = 1; i <= n; i++) {
        cout << '\t' << i;
    }
    cout << '\n';
    for (int i = 1; i <= n; i++) {
        cout << '\t' << dist[i];
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

    dijkstra(g, n, a, b);

    fin.close();
    return 0;
}