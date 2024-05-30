#include <iostream>
#include <fstream>
#include <vector>
#include <stack>

using namespace std;

typedef pair<int, int> pii;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<pii> vii;
typedef vector<vii> vvii;

#define INF 1e9

void dfs(int node, vvi &g, vb &visited, stack<int> &st) {
    visited[node] = true;
    for (const int &neigh: g[node])
        if (!visited[neigh])
            dfs(neigh, g, visited, st);
    st.push(node);
}

int MaxLengthPathInDag(vvi &g, int n, int start) {
    vb visited(n+1, false);
    stack<int> st;
    vi dist(n+1, -INF), parent(n+1, -1);
    int maxDist = -INF;

    for (int i=1; i<=n; i++)
        if (!visited[i])
            dfs(i, g, visited, st);

    dist[start] = 0;
    parent[start] = start;
    while (!st.empty()) {
        int node = st.top();
        st.pop();

        for (const int &neigh: g[node]) {
            dist[neigh] = max(dist[neigh], dist[node] + 1);
            if (dist[neigh] == dist[node] + 1)
                parent[neigh] = node;
        }
    }

    int end = 0;
    for (int i = 1; i <= n; i++) {
        if (dist[i] > maxDist) {
            maxDist = dist[i];
            end = i;
        }
    }

    vi path;
    int node = end;
    while (node != parent[node]) {
        path.push_back(node);
        node = parent[node];
    }
    path.push_back(start);

    for (int i = path.size()-1; i >= 0; i--)
        cout << path[i] << ' ';
    cout << '\n';

    return maxDist;
}

int main() {
    ifstream fin ("MaxLengthPathInDAG.in");
    int n, m, start;
    vvi g;

    fin >> n >> m >> start;
    g = vvi(n+1);
    while (m--) {
        int x, y;
        fin >> x >> y;
        g[x].push_back(y);
    }

    cout << MaxLengthPathInDag(g, n, start) << '\n';

    fin.close();
    return 0;
}