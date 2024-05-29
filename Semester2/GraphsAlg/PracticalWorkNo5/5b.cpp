#include <iostream>
#include <vector>
#include <limits>
#include <fstream>
#include <queue>
        
using namespace std;

const int INF = numeric_limits<int>::max();

// Edge structure to represent a directed edge with a weight
struct Edge {
    int from;
    int to;
    int weight;

    Edge(int from, int to, int weight) : from(from), to(to), weight(weight) {}
};

// Function to perform the Bellman-Ford algorithm
vector<int> bellmanFord(int V, const vector<Edge> &edges, int src) {
    vector<int> dist(V+1, INF); // Initialize distances to infinity
    dist[src] = 0; // Distance from source to itself is 0

    queue<int> q;
    vector<bool> inQueue(V, false); // Keeps track of whether a vertex is in the queue

    q.push(src);
    inQueue[src] = true;

    vector<int> relaxCount(V, 0); // Keeps track of relaxation count for each vertex

    while (!q.empty()) {
        int u = q.front();
        q.pop();
        inQueue[u] = false;

        for (const auto &edge : edges) {
            if (edge.from == u) {
                int v = edge.to;
                int weight = edge.weight;

                if (dist[u] != INF && dist[u] + weight < dist[v]) {
                    dist[v] = dist[u] + weight;
                    relaxCount[v]++; // Increment relaxation count for vertex v

                    if (relaxCount[v] >= V) {
                        // Detected a negative cycle reachable from the source vertex
                        cout << "Graph contains a negative cycle\n";
                        return vector<int>(); // Return an empty vector to indicate failure
                    }

                    q.push(v);
                }
            }
        }
    }

    return dist;
}

int main() {
    ifstream fin("input-5.txt");
    int V, E; // Number of vertices and edges
    int src, dest; // Source and destination vertices
    fin >> V >> E >> src >> dest;

    vector<Edge> edges;
    for (int i = 0; i < E; ++i) {
        int from, to, weight;
        fin >> from >> to >> weight;
        edges.push_back(Edge(from, to, weight));
    }

    vector<int> shortestDistances;
    for (int i = 1; i <= E; i++)
        shortestDistances = bellmanFord(V, edges, src);


    if (!shortestDistances.empty()) {
        // Print shortest distance from source vertex to destination vertex
        cout << "Minimum cost from vertex " << src << " to vertex " << dest << " is ";
        if (shortestDistances[dest] == INF) {
            cout << "INF\n";
        } else {
            cout << shortestDistances[dest] << "\n";
        }
    }

    fin.close();
    return 0;
}
