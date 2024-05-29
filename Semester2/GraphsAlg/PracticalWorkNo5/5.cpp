#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <string>
#include <stack>
#include <queue>
#include <climits>

using namespace std;

ifstream fin ("input-5.txt");

typedef pair<int, int> pii;
typedef vector<int> vi;
typedef vector<bool> vb;
typedef vector<pii> vii;
typedef vector<vii> vvii;


void dijkstra(vvii &g, int n, int a, int b) {
    priority_queue<pii, vii, greater<pii>> q;
    vi dist(n + 1, INT_MAX), visited(n + 1, 0);

    dist[a] = 0;
    q.push({0, a});

    while (!q.empty()) {
        int u = q.top().second;
        int d = q.top().first;
        q.pop();

        if (d > dist[u]) continue;

        visited[u]++;
        if (visited[u] >= n) {
            cout << "Negative cycle detected\n";
            return;
        }

        for (auto &[neigh, cost]: g[u]) {
            if (dist[u] + cost < dist[neigh]) {
                dist[neigh] = dist[u] + cost;
                q.push({dist[neigh], neigh});
            }
        }
    }

    cout << "The shortest path from " << a << " to " << b << " is " << dist[b] << endl;
}

void back(int u, int d, int b, vi &visited, int &min_cost, vvii &g) {
    if (u == b) {
        min_cost = min(min_cost, d);
        return;
    }

    visited[u]++;
    for (auto &[neigh, cost]: g[u]) {
        if (visited[u] > g.size()) {
            cout << "Negative cycle detected\n";
            return;
        }
        back(neigh, d + cost, b, visited, min_cost, g);
    }
    visited[u]--;
}




int main() { 
    int n, m, a, b;
    vvii g;

    fin >> n >> m >> a >> b;
    g = vvii(n + 1);
    while (m--) {
        int x, y, c;
        fin >> x >> y >> c;
        g[x].push_back({y, c});
    }   

    // Complexity: O((V + E) * log(V))
    // dijkstra(g, n, a, b);

    // Exponential complexity O(V^E)
    vi visited(n + 1, 0);
    int min_cost = INT_MAX;
    back(a, 0, b, visited, min_cost, g);
    cout << "The shortest path from " << a << " to " << b << " is " << min_cost << endl;

    fin.close();
    return 0;
}