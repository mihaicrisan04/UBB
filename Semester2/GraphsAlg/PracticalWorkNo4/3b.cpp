#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <queue>
#include <stack>

using namespace std;


bool isCyclicUtil(vector<vector<pair<int, int>>> &g, int node, vector<bool> &visited, vector<bool> &recStack) {
    visited[node] = true;
    recStack[node] = true;

    for (auto &neigh: g[node]) {
        if (!visited[neigh.first] && isCyclicUtil(g, neigh.first, visited, recStack)) {
            return true;
        } 
        else if (recStack[neigh.first]) {
            return true;
        }
    }

    recStack[node] = false;
    return false;
}

bool isDAG(vector<vector<pair<int,int>>> &g, int n) {
    vector<bool> visited(n+1, false);
    vector<bool> recStack(n+1, false);

    for (int i = 1; i <= n; i++) {
        if (!visited[i]) {
            if (isCyclicUtil(g, i, visited, recStack)) {
                return false;
            }
        }
    } 
    
    return true;
}  

void dfs(vector<vector<pair<int, int>>> &g, int node, vector<bool> &visited, stack<int> &s) {
    visited[node] = true;

    for (auto &neigh: g[node]) {
        if (!visited[neigh.first]) {
            dfs(g, neigh.first, visited, s);
        }
    }

    s.push(node);
}

vector<int> topologicalSort(vector<vector<pair<int, int>>> &g, int n) {
    vector<bool> visited(n+1, false);
    stack<int> s; 

    for (int i = n; i >= 1; i--) {
        if (!visited[i]) {
            dfs(g, i, visited, s);
        }
    }

    vector<int> topOrder;
    while (!s.empty()) {
        topOrder.push_back(s.top());
        s.pop();
    }

    return topOrder;
}

const int INF = 1e9;

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
    ifstream fin ("input-3b.txt");
    int n, m, a, b;
    vector<vector<pair<int,int>>> g;

    fin >> n >> m >> a >> b;
    g = vector<vector<pair<int, int>>>(n+1);

    for (int i = 0; i < m; i++) {
        int x, y, c;
        fin >> x >> y >> c;
        g[x].push_back({y, c});
    }

    if (!isDAG(g, n)) {
        cout << "Graph is not a DAG\n";
        return 0;
    }

    cout << "Topological sort:\n";
    vector<int> topOrder = topologicalSort(g, n);
    for (int i: topOrder) {
        cout << i << '\n';
    }

    num_min_walks(g, n, a, b);

    fin.close();
    return 0;
}