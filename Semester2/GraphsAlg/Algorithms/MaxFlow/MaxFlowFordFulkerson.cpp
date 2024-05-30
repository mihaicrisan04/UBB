#include <iostream>
#include <fstream>
#include <vector>
#include <queue>

using namespace std;

typedef pair<int, int> pii;
typedef vector<int> vi;
typedef vector<bool> vb;
typedef vector<pii> vii;
typedef vector<vii> vvii;

#define INF 1e9

bool bfs(vvii &g, int s, int t, vi &p) {
    int n = g.size();
    vb visited(n, false);
    queue<int> q;

    q.push(s);
    visited[s] = true;
    p[s] = -1;

    while (!q.empty()) {
        int u = q.front();
        q.pop();

        for (auto &[v, w] : g[u]) {
            if (!visited[v] && w > 0) {
                visited[v] = true;
                p[v] = u;
                q.push(v);
                if (v == t) return true;
            }
        }
    }
    return false;
}

int maxFlow(vvii &g, int s, int t) {
    int n = g.size();
    int max_flow = 0;

    vvii r = g;
    vi p(n, -1);
    
    // Augment the flow while there is a path from source to sink
    while (bfs(r, s, t, p)) {
        // Find the minimum residual capacity of the edges along the path
        int path_flow = INF;
        for (int v = t; v != s; v = p[v]) {
            int u = p[v];
            for (auto &[k, w] : r[u]) {
                if (k == v) {
                    path_flow = min(path_flow, w);
                    break;
                }
            }
        }

        // Update residual capacities of the edges and reverse edges along the path
        for (int v = t; v != s; v = p[v]) {
            int u = p[v];
            for (int k = 0; k < r[u].size(); k++) {
                if (r[u][k].first == v) {
                    r[u][k].second -= path_flow;
                    break;
                }
            }
            for (int k = 0; k < r[v].size(); k++) {
                if (r[v][k].first == u) {
                    r[v][k].second += path_flow;
                    break;
                }
            }
        }

        // Add path flow to overall flow
        max_flow += path_flow;
    }
    return max_flow;
}

int main() {
    ifstream fin("MaxFlowFordFulkerson.in");

    int n, m, s, t;
    vvii g;

    fin >> n >> m >> s >> t;
    g = vvii(n);
    while (m--) {
        int u, v, w;
        fin >> u >> v >> w;
        g[u].push_back({v, w});
    }

    int max_flow = maxFlow(g, s, t);
    cout << max_flow;

    fin.close();
    return 0;
}