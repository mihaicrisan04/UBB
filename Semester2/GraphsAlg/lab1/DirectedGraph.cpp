#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <set>
#include <random>

using namespace std;

// Abstract data type for Edge_id
class Edge_id {
private:
    int source;
    int target;

public:
    // Constructor to initialize the source and target vertices of the edge
    Edge_id(int source, int target) : source(source), target(target) {}

    // Getter methods to retrieve source and target vertices
    int getSource() const {
        return source;
    }

    int getTarget() const {
        return target;
    }

    // Overload comparison operators to compare Edge_ids
    bool operator==(const Edge_id& other) const {
        return source == other.source && target == other.target;
    }

    bool operator!=(const Edge_id& other) const {
        return !(*this == other);
    }
};


// Abstract data type for representing a directed graph
class DirectedGraph {
private:
    int numVertices;
    vector<set<int>> adjList;
    map<Edge_id, int> edgeCosts;

public:
    // Constructor
    DirectedGraph(int n) : numVertices(n), adjList(n) {}

    // Function to get the number of vertices
    int getNumVertices() const {
        return numVertices;
    }

    // Function to add an edge to the graph
    void addEdge(int source, int target, int cost) {
        adjList[source].insert(target);
        edgeCosts[Edge_id(source, target)] = cost;
    }

    // Function to remove an edge from the graph
    void removeEdge(int source, int target) {
        adjList[source].erase(target);
        edgeCosts.erase(Edge_id(source, target));
    }

    // Iterator class to iterate over vertices
    class VertexIterator {
    private:
        const DirectedGraph& graph;
        int currentVertex;

    public:
        VertexIterator(const DirectedGraph& g, int v) : graph(g), currentVertex(v) {}

        // Prefix increment operator (++iter)
        VertexIterator& operator++() {
            ++currentVertex;
            return *this;
        }

        // Dereference operator (*iter)
        int operator*() const {
            return currentVertex;
        }

        // Inequality operator (iter1 != iter2)
        bool operator!=(const VertexIterator& other) const {
            return currentVertex != other.currentVertex;
        }
    };

    // Function to begin iterating over vertices
    DirectedGraph::VertexIterator DirectedGraph::beginVertices() {
        return VertexIterator(*this, 0);
    }

    // Function to end iteration over vertices
    DirectedGraph::VertexIterator DirectedGraph::endVertices() {
        return VertexIterator(*this, numVertices);
    }


    // Function to check if there is an edge from source to target
    bool hasEdge(int source, int target) const {
        return adjList[source].count(target) > 0;
    }

    // Function to retrieve the Edge_id of an edge
    Edge_id getEdgeId(int source, int target) const {
        // Implement logic to retrieve Edge_id
    }

    // Function to get the in-degree of a vertex
    int getInDegree(int vertex) const {
        // Implement logic to calculate in-degree
    }

    // Function to get the out-degree of a vertex
    int getOutDegree(int vertex) const {
        // Implement logic to calculate out-degree
    }

    // Function to parse (iterate) the set of outbound edges of a specified vertex
    class OutEdgeIterator {
        // Implement iterator logic
    };

    OutEdgeIterator beginOutEdges(int vertex) {
        // Implement iterator logic
    }

    OutEdgeIterator endOutEdges(int vertex) {
        // Implement iterator logic
    }

    // Function to parse the set of inbound edges of a specified vertex
    class InEdgeIterator {
        // Implement iterator logic
    };

    InEdgeIterator beginInEdges(int vertex) {
        // Implement iterator logic
    }

    InEdgeIterator endInEdges(int vertex) {
        // Implement iterator logic
    }

    // Function to retrieve the endpoints of an edge specified by an Edge_id
    pair<int, int> getEndpoints(Edge_id edge) const {
        // Implement logic to retrieve endpoints
    }

    // Function to retrieve or modify the information attached to a specified edge
    int getEdgeCost(Edge_id edge) const {
        return edgeCosts.at(edge);
    }

    void setEdgeCost(Edge_id edge, int cost) {
        edgeCosts[edge] = cost;
    }

    // Function to read the graph from a text file
    static DirectedGraph readGraphFromFile(const string& filename) {
        ifstream file(filename);
        if (!file.is_open()) {
            throw runtime_error("Failed to open file: " + filename);
        }

        int numVertices, numEdges;
        file >> numVertices >> numEdges;

        DirectedGraph graph(numVertices);

        for (int i = 0; i < numEdges; ++i) {
            int source, target, cost;
            file >> source >> target >> cost;
            graph.addEdge(source, target, cost);
        }

        file.close();
        return graph;
    }

    // Function to write the graph to a text file
    void writeGraphToFile(const string& filename) const {
        ofstream file(filename);
        if (!file.is_open()) {
            throw runtime_error("Failed to create file: " + filename);
        }

        file << numVertices << " " << edgeCosts.size() << endl;
        for (int source = 0; source < numVertices; ++source) {
            for (int target : adjList[source]) {
                file << source << " " << target << " " << edgeCosts.at(Edge_id(source, target)) << endl;
            }
        }

        file.close();
    }

    static DirectedGraph readGraphFromFile(const string& filename) {
        ifstream file(filename);
        if (!file.is_open()) {
            throw runtime_error("Failed to open file: " + filename);
        }

        int numVertices, numEdges;
        file >> numVertices >> numEdges;

        DirectedGraph graph(numVertices);

        for (int i = 0; i < numEdges; ++i) {
            int source, target, cost;
            file >> source >> target >> cost;
            graph.addEdge(source, target, cost);
        }

        file.close();
        return graph;
    }

    // Function to create a random graph with specified number of vertices and edges
    static DirectedGraph createRandomGraph(int numVertices, int numEdges) {
        DirectedGraph graph(numVertices);

        int edges = 0;
        while (edges < numEdges) {
            int source = rand() % numVertices;
            int target = rand() % numVertices;
            int cost = rand() % 100;
            if (graph.hasEdge(source, target) || source == target) {
                continue;
            }
            graph.addEdge(source, target, cost);
            edges++;
        }

        return graph;
    }
};

int main() {
    // Example usage:
    DirectedGraph graph1 = DirectedGraph::readGraphFromFile("graph.txt");
    DirectedGraph graph2 = DirectedGraph::createRandomGraph(10, 20);

    // Use the graph...

    return 0;
}
