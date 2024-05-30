# Graph's Cheat Sheet

## Typedefs and Consts

```cpp
typedef pair<int, int> pii;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<pii> vii;
typedef vector<vii> vvii;

#define INF 1e9
```

## Dijkstra's Algorithm

```cpp
void dijkstra(vvii &g, int n, int a, int b) {
    priority_queue<pii, vii, greater<pii>> pq;
    vi dist(n + 1, INT_MAX);

    dist[a] = 0;
    pq.push({0, a});

    while (!pq.empty()) {
        int u = pq.top().second;
        int d = pq.top().first;
        pq.pop();

        // skip the node if a shorter distance was already found
        if (d > dist[u]) continue;

        for (auto &[neigh, cost]: g[u]) {
            if (dist[u] + cost < dist[neigh]) {
                dist[neigh] = dist[u] + cost;
                pq.push({dist[neigh], neigh});
            }
        }
    }

    cout << "The shortest path from " << a << " to " << b << " is " << dist[b] << endl;
}
```

## Bellman-Ford's Algorithm

```cpp
int n;
bool negative_cycle = false;
vvii g(N);
vi dist(N, INF), visited(N, 0);

void bellman_ford() {
    queue<int> q;

    dist[1] = 0;
    q.push(1);

    while (!q.empty() && !negative_cycle) {
        int u = q.front();
        q.pop();

        visited[u]++;
        if (visited[u] == n) {
            negative_cycle = true;
            return;
        }

        for (auto &[neigh, weight]: g[u]) {
            if (dist[neigh] > dist[u] + weight) {
                dist[neigh] = dist[u] + weight;
                q.push(neigh);
            }
        }
    }
}
```

## Kruskal's Algorithm

```cpp
struct Edge {
    int a, b;
    int c;
};

int n, mstWeight;
vector<Edge> edges;
vi t(NMAX, -1), r(NMAX, 0);

int root(int node) {
    if (t[node] == -1)
        return node;
    return root(t[node]);
}

void unite(int ra, int rb) {
    if (r[ra] < r[rb]) {
        t[ra] = rb;
    }
    else {
        t[rb] = ra;
        if (r[ra] == r[rb]) {
            r[ra]++;
        }
    }
}

int Kruskal() {
    sort(edges.begin(), edges.end(), [](const Edge &a, const Edge &b) {
        return a.c < b.c;
    });

    for (int i = 0; i < edges.size(); i++) {
        int ra = root(edges[i].a);
        int rb = root(edges[i].b);
        if (ra != rb) {
            unite(ra, rb);
            sol += edges[i].c;
        }
    }

    return mstWeight;
}
```

## Prims's Algorithm

```cpp
int prim(vvii &g, vi &t, int start) {
    int n = g.size();
    int mstWeight = 0;
    vb visited(n+1, false);
    vi key(n+1, INF); // used to make the dads vector
    priority_queue<pii, vector<pii>, greater<pii>> pq; // priority queue to select the next edge to add to the MST

    t[start] = 0;
    pq.push({0, start});

    while (!pq.empty()) {
        int u = pq.top().second;
        int w = pq.top().first;
        pq.pop();

        if (visited[u]) continue; // skip visited vertices

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
```

## Topological Sort

```cpp
int n;
vvi g(NMAX);
vb viz(NMAX, false);
stack<int> st;

void dfs(int node) {
    viz[node] = true;

    for (int i : g[node])
        if (!viz[i])
            dfs(i);

    st.push(node);
}

void top_sort() {
    for (int i = 1; i <= n; i++)
        if (!viz[i])
            dfs(i);
    
    // write the nodes in sorted topological order
    while (!st.empty()) {
        cout << st.top() << ' ';
        st.pop();
    } 
}
```

## Kosraju's Algorithm

```cpp
void dfs(int node, vector<vector<int>> &g, vector<bool> &visited, stack<int> &st) {
    visited[node] = true;
    for (auto neigh: g[node]) {
        if (!visited[neigh]) {
            dfs(neigh, g, visited, st);
        }
    }
    st.push(node);
}

void dfs2(int node, vector<vector<int>> &g, vector<bool> &visited) {
    visited[node] = true;
    cout << node << " ";
    for (auto neigh : g[node]) {
        if (!visited[neigh]) {
            dfs2(neigh, g, visited);
        }
    }
}

// Complexity O(V + E)
void kosaraju(int n, vvi &g, vvi &gt) {
    stack<int> st;
    vb visited(n + 1, false);

    // First DFS to get the order of the nodes in topological order
    for (int i = 1; i <= n; i++) {
        if (!visited[i]) {
            dfs(i, g, visited, st);
        }
    }

    // Reset visited
    for (int i = 1; i <= n; i++) visited[i] = false;

    // go through the stack and get the SCCs
    while (!st.empty()) {
        int v = st.top();
        st.pop();

        // DFS on the transpose to get an SCC
        if (!visited[v]) {
            dfs2(v, gt, visited);
            cout << "\n";
        }
    }
}
```

## Maximum Length Path in DAG in O(n + m)

```cpp
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
```

## Maximum Flow Ford-Fulkerson Algorithm

1. Start with initial flow as 0.
2. While there exists an augmenting path from the source to the sink:  
    - Find an augmenting path using any path-finding algorithm, such as breadth-first search or depth-first search.
    - Determine the amount of flow that can be sent along the augmenting path, which is the minimum residual capacity along the edges of the path.
    - Increase the flow along the augmenting path by the determined amount.
3. Return the maximum flow.

```cpp
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

// The weight represents the capacity of the edge from u to v 
int maxFlow(vvii &g, int s, int t) {
    int n = g.size();
    int max_flow = 0;

    // Residual graph, with residual capacities as the capacities in the original graph
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
```

## Maximum Flow Minimum Cost

```cpp

```
