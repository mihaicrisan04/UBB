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

1. Initialize a list of `dist` with `INF` and the priority queue to be used as a min heap
2. Set the `dist[source]` to `0` and push the pair of `{dist, node}` into the queue to start
3. For each of the current vertex's unvisited neighbour update the distances and push the pair into the queue for further computations
4. In the end we are left with the shortest path from the source vertex to every other vertex in the graph.

**NOTE:** using the priority queue ensures that only the shortest distances are taken into account first, so in this version of the algorithm there is no need for a visited vector (also dijkstra only works for non negative weights!)

```cpp
void dijkstra(vvii &g, int n, int source) {
    priority_queue<pii, vii, greater<pii>> pq;
    vi dist(n + 1, INF);

    dist[source] = 0;
    pq.push({0, source});

    while (!pq.empty()) {
        int u = pq.top().second;
        int d = pq.top().first;
        pq.pop();

        if (d > dist[u]) continue;

        for (auto &[neigh, weight]: g[u]) {
            if (dist[neigh] > dist[u] + weight) {
                dist[neigh] = dist[u] + weight;
                pq.push({dist[neigh], neigh});
            }
        }
    }
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
void dfs(int node, vvi &g, vector<bool> &visited, stack<int> &st) {
    visited[node] = true;
    for (auto neigh: g[node]) {
        if (!visited[neigh]) {
            dfs(neigh, g, visited, st);
        }
    }
    st.push(node);
}

void dfs2(int node, vvi &g, vector<bool> &visited) {
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

## Earliest and Latest Scheduling + Critical Activities

```cpp
void dfs(int u, vvi &gout, vb &visited, stack<int> &st) {
    visited[u] = true;
    for (int v : gout[u]) {
        if (!visited[v]) {
            dfs(v, gout, visited, st);
        }
    }
    st.push(u);
}

void Schedule(int n, vvi &gin, vvi &gout, vi &costs) {
    stack<int> st;
    vb visited(n + 1, false), critical(n + 1, false);
    vi es(n + 1, 0), ef(n + 1, 0), ls(n + 1, INF), lf(n + 1, INF);
    vi topSort;
    int maxEf = 0;

    for (int i = 1; i < n; i++) {
        if (!visited[i]) {
            dfs(i, gout, visited, st);
        }
    }

    topSort.push_back(0); // dummy node
    while (!st.empty()) {
        topSort.push_back(st.top());
        st.pop();
    }

    for (int i = 1; i <= n; i++) {
        int u = topSort[i];
        for (int v : gin[u]) {
            es[u] = max(es[u], ef[v]);
        }
        ef[u] = es[u] + costs[u];
        maxEf = max(maxEf, ef[u]);
    }

    for (int i = 1; i <= n; i++) {
        if (gout[i].size() == 0) {
            lf[i] = maxEf;
        }
    }
    for (int i = n; i >= 1; i--) {
        int u = topSort[i];
        for (int v : gout[u]) {
            lf[u] = min(lf[u], ls[v]);
        }
        ls[u] = lf[u] - costs[u];
    }

    for (int i = 1; i <= n; i++) {
        if (es[i] == ls[i] && ef[i] == lf[i]) {
            critical[i] = true;
        }
    }

    for (int i = 1; i <= n; i++) {
        cout << (char)('A' + i - 1) << " " << es[i] << " " << ef[i] << " " << ls[i] << " " << lf[i] << " " << critical[i] << endl;
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

## Shortest Cycle in a Directed Graph

```cpp
vi findShortestCycle(const vvi &graph) {
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
```

## Polynomial Shortest Cycle in Directed Graph With Negative Cost

```cpp
struct Edge {
    int u, v, cost;
};

vi findNegativeCycle(int V, int s, vector<Edge>& edges) {
    // Distance table to store shortest paths with exactly k edges
    // Layered V times to ensure a full graph traversal is done
    vvi dist(V + 1, vi(V, INF));
    vvi parent(V + 1, vi(V, -1));
    dist[0][s] = 0;

    // Relax edges up to V times (basically traverse the graph and update the distance and parent vectors)
    for (int k = 1; k <= V; ++k) {
        for (const auto& edge : edges) {
            if (dist[k-1][edge.u] != INF && dist[k-1][edge.u] + edge.cost < dist[k][edge.v]) {
                dist[k][edge.v] = dist[k-1][edge.u] + edge.cost;
                parent[k][edge.v] = edge.u;
            }
        }
    }

    // Check for negative cycle
    if (dist[V][s] < dist[V-1][s]) {
        // A negative cycle exists, trace it back
        vi cycle;
        int cycle_vertex = s;

        // Extract the cycle
        do {
            cycle.push_back(cycle_vertex);
            cycle_vertex = parent[V][cycle_vertex];
        } while (cycle_vertex != s);
        cycle.push_back(s);
        reverse(cycle.begin(), cycle.end());

        return cycle;
    }

    // No negative cycle found
    return vi{};
}
```

## Maximum Flow Ford-Fulkerson Algorithm + Min Cut

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
                if (v == t) return true;
                q.push(v);
            }
        }
    }
    return false;
}

// DFS to mark all reachable vertices from the source
void dfs(vvii &r, int s, vb &visited) {
    visited[s] = true;
    for (auto &[v, w] : r[s]) {
        if (w > 0 && !visited[v]) { // Only consider edges with positive residual capacity
            dfs(r, v, visited);
        }
    }
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

        // Find all reachable vertices from source
        vb visited(n, false);
        dfs(r, s, visited);

        // Find the minimum cut edges
        for (int u = 0; u < n; ++u) {
            if (visited[u]) { // u is reachable from the source
                for (auto &[v, w] : g[u]) {
                    if (!visited[v] && w > 0) { // v is not reachable and there is an edge u -> v in the original graph
                        min_cut.push_back({u, v});
                    }
                }
            }
        }
    }
    return max_flow;
}
```

## Minimum Cost Maximum Flow

```py
class MCMFGraph:
    def __init__(self, vertices):
        self.V = vertices
        self.adj = [[] for _ in range(vertices)]
        self.cap = {}
        self.cost = {}
        self.flow = {}

    def add_edge(self, u, v, capacity, cost):
        self.adj[u].append(v)
        self.adj[v].append(u)
        self.cap[(u, v)] = capacity
        self.cap[(v, u)] = 0
        self.cost[(u, v)] = cost
        self.cost[(v, u)] = -cost
        self.flow[(u, v)] = 0
        self.flow[(v, u)] = 0

    def bellman_ford(self, src, sink):
        dist = [float('inf')] * self.V
        parent = [-1] * self.V
        in_queue = [False] * self.V

        dist[src] = 0
        queue = deque([src])
        in_queue[src] = True

        while queue:
            u = queue.popleft()
            in_queue[u] = False

            for v in self.adj[u]:
                if self.cap[(u, v)] > self.flow[(u, v)] and dist[v] > dist[u] + self.cost[(u, v)]:
                    dist[v] = dist[u] + self.cost[(u, v)]
                    parent[v] = u
                    if not in_queue[v]:
                        queue.append(v)
                        in_queue[v] = True

        return dist, parent

    def min_cost_max_flow(self, source, sink):
        max_flow = 0
        min_cost = 0

        while True:
            dist, parent = self.bellman_ford(source, sink)
            if dist[sink] == float('inf'):
                break

            path_flow = float('inf')
            s = sink
            while s != source:
                path_flow = min(path_flow, self.cap[(parent[s], s)] - self.flow[(parent[s], s)])
                s = parent[s]

            max_flow += path_flow
            min_cost += path_flow * dist[sink]

            v = sink
            while v != source:
                u = parent[v]
                self.flow[(u, v)] += path_flow
                self.flow[(v, u)] -= path_flow
                v = parent[v]

        return max_flow, min_cost
```
