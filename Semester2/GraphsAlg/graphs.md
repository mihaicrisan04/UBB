# Graph's Cheat Sheet

## Typedefs and Consts

```cpp
typedef pair<int, int> pii;
typedef vector<int> vi;
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


```

## Maximum Flow

```cpp

```
