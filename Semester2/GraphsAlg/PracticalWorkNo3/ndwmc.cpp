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


/*
    Assumming there are no infinite cycles in the graph, we can use Dijkstra's algorithm to find the shortest path from node a to node b.
*/
void num_min_walks(vector<vector<pair<int,int>>>& g, int n, int a, int b) {
    vector<int> dist(n+1, INF);
    vector<int> visited(n+1, 0);
    vector<int> count(n+1, 0);
    priority_queue<pair<int, int>, vector<pair<int, int>>, cmp> h;

    dist[a] = 0;
    count[a] = 1;
    h.push({0, a});

    while (!h.empty()) {
        int curr = h.top().second;
        h.pop();

        visited[curr]++;

        if (visited[curr] > n) {
            cout << "The graph contains an infinite cycle\n";
            return;
        }

        for (const auto& i : g[curr]) {
            int neigh = i.first;
            int cost = i.second;

            if (dist[neigh] == dist[curr] + cost) {
                count[neigh] += count[curr];
            }
            else if (dist[neigh] > dist[curr] + cost) {
                count[neigh] = count[curr];
                dist[neigh] = dist[curr] + cost;
                h.push({dist[neigh], neigh});
            }
        }
    }
    cout << "The number of minimum walks from node " << a << " to node " << b << " is " << count[b] << " of distance " << dist[b] << '\n';
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

    num_min_walks(g, n, a, b);

    fin.close();
    return 0;
}