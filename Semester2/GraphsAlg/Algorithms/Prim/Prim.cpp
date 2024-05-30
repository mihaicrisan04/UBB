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


int prim(vvii &g, vi &t, int start) {
    int n = g.size();
    int mstWeight = 0;
    vb visited(n+1, false);
    vi key(n+1, 1e9);
    priority_queue<pii, vector<pii>, greater<pii>> pq;

    t[start] = 0;
    pq.push({0, start});

    while (!pq.empty()) {
        int u = pq.top().second;
        int w = pq.top().first;
        pq.pop();

        if (visited[u]) continue;

        visited[u] = true;
        mstWeight += w;

        for (auto &[neigh, weight]: g[u]) {
            if (!visited[neigh]) {
                if (key[neigh] > weight) {
                    key[neigh] = weight;    
                    t[neigh] = u; 
                }
                pq.push({weight, neigh});
            }
        }
    }

    return mstWeight;
}

int main() {
    ifstream fin ("prim.in");
    int n, m;
    vvii g; 
    vi t;

    fin >> n >> m;
    g = vvii(n+1);
    t = vi(n+1, 0);
    while (m--) {
        int u, v, w;
        fin >> u >> v >> w;
        g[u].push_back({v, w});
        g[v].push_back({u, w});
    }

    int res = prim(g, t, 1);
    
    cout << res << '\n';
    for (int i = 1; i <= n; i++) {
        cout << t[i] << ' ';
    }

    fin.close();
    return 0;
}