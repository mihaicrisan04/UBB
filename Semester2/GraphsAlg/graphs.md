# Graph's Cheat Sheet

## Typedefs
```cpp
typedef pair<int, int> pii;
typedef vector<int> vi;
typedef vector<bool> vb;
typedef vector<pii> vii;
typedef vector<vii> vvii;
```

# Dijkstra's Algorithm
```cpp
void dijkstra(vvii &g, int n, int a, int b) {
    priority_queue<pii, vii, greater<pii>> q;
    vi dist(n + 1, INT_MAX);

    dist[a] = 0;
    q.push({0, a});

    while (!q.empty()) {
      int u = q.top().second;
      int d = q.top().first;
      q.pop();

      // skip the node if a shorter distance was already found
      if (d > dist[u]) continue;

      for (auto &[neigh, cost]: g[u]) {
        if (dist[u] + cost < dist[neigh]) {
            dist[neigh] = dist[u] + cost;
            q.push({dist[neigh], neigh});
        }
      }
    }

    cout << "The shortest path from " << a << " to " << b << " is " << dist[b] << endl;
}
```

# Kruskal's Algorithm

```cpp
struct Edge {
    int a, b;
    int c;
};

int n, sol;
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

void Kruskal() {
    sort(edges.begin(), edges.end(), [](const Edge &a, const Edge &b) {return a.c < b.c;});

    for (int i = 0; i < edges.size(); i++) {
        int ra = root(edges[i].a);
        int rb = root(edges[i].b);
        if (ra != rb) {
            unite(ra, rb);
            sol += edges[i].c;
        }
    }

    // sol is the total path cost between all the vertices
    cout << sol;
}
```

# Topological Sort

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

# Prims's Algorithm

```cpp

```

# Bellman-Ford's Algorithm

```cpp
int n;
bool negative_cycle = false;
vvii g(DIM);
vi dist(DIM, INF), viz(DIM);

void bellman_ford() {
    queue<int> q;

    dist[1] = 0;
    q.push(1);

    while (!q.empty() && !negative_cycle) {
        int u = q.front();
        q.pop();
        viz[u]++;

        if (viz[u] == n) {
            negative_cycle = true;
            return;
        }

        for (const auto& neigh: g[u]) {
            int vecin = neigh.first;
            int cost = neigh.second;

            if (dist[vecin] > dist[u] + cost) {
                dist[vecin] = dist[u] + cost;
                q.push(vecin);
            }
        }
    }
}
```

# Kosraju's Algorithm

```cpp


```