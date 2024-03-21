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

    bool operator<(const Edge_id& other) const {
        return source < other.source || (source == other.source && target < other.target);
    }

    bool operator>(const Edge_id& other) const {
        return source > other.source || (source == other.source && target > other.target);
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
    DirectedGraph(int n) : numVertices(n), adjList(n+1) {}

    // Copy constructor
    DirectedGraph(const DirectedGraph& other) 
                : numVertices(other.numVertices),
                  adjList(other.adjList),
                  edgeCosts(other.edgeCosts) {}

    // Function to get the number of vertices
    int getNumVertices() const {
        return numVertices;
    }

    // Function to add a vertex to the graph
    void addVertex() {
        numVertices++;
        adjList.push_back(set<int>());
    }

    // Function to remove a vertex from the graph
    void removeVertex(int vertex) {
        while (adjList[vertex].size() > 0) {
            int target = *adjList[vertex].begin();
            removeEdge(vertex, target);
        }

        for (int i = 0; i < numVertices; i++) {
            if (i == vertex) {
                continue;
            }
            if (adjList[i].count(vertex) > 0) {
                removeEdge(i, vertex);
            }
        }
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
        int it;

    public:
        VertexIterator(const DirectedGraph& g, int v) : graph(g), it(v) {}

        // Prefix increment operator (++iter)
        VertexIterator& operator++() {
            ++it;
            return *this;
        }

        // Dereference operator (*iter)
        int operator*() const {
            return it;
        }

        // Inequality operator (iter1 != iter2)
        bool operator!=(const VertexIterator& other) const {
            return it != other.it;
        }
    };

    // Function to begin iterating over vertices
    VertexIterator beginVertices() {
        return VertexIterator(*this, 0);
    }

    // Function to end iteration over vertices
    VertexIterator endVertices() {
        return VertexIterator(*this, numVertices);
    }


    // Function to check if there is an edge from source to target
    bool hasEdge(int source, int target) const {
        return adjList[source].count(target) > 0;
    }

    // Function to retrieve the Edge_id of an edge
    Edge_id getEdgeId(int source, int target) const {
        for (const auto& item : edgeCosts) {
            Edge_id edge = item.first;
            if (edge == Edge_id(source, target)) {
                return edge;
            }
        }
        throw runtime_error("Edge not found");
    }

    // Function to get the in-degree of a vertex
    int getInDegree(int vertex) const {
        int inDegree = 0;
        for (int i = 0; i < numVertices; i++) {
            if (i == vertex) {
                continue;
            }   
            inDegree += adjList[i].size();
        }
        return inDegree;
    }

    // Function to get the out-degree of a vertex
    int getOutDegree(int vertex) const {
        return adjList[vertex].size();
    }

    // Function to parse (iterate) the set of outbound edges of a specified vertex
    class OutEdgeIterator {
        private:
            set<int>::iterator it;
        public:
            OutEdgeIterator(const set<int>::iterator start) : it(start) {}

            // Prefix increment operator (++iter)
            OutEdgeIterator& operator++() {
                ++it;
                return *this;
            }

            // Dereference operator (*iter)
            int operator*() const {
                return *it;
            }

            // Inequality operator (iter1 != iter2)
            bool operator!=(const OutEdgeIterator& other) const {
                return it != other.it;
            }
    };
 
    OutEdgeIterator beginOutEdges(int vertex) {
        return OutEdgeIterator(adjList[vertex].begin());
    }

    OutEdgeIterator endOutEdges(int vertex) {
        return OutEdgeIterator(adjList[vertex].end());
    }


    class InEdgeIterator {
        private:
            const DirectedGraph& graph;
            int vertex; 
            int i;

        public:
            InEdgeIterator(const DirectedGraph& g, int v, int i) : graph(g), vertex(v), i(i) {}

            // Prefix increment operator (++iter)
            InEdgeIterator& operator++() {
                do {
                    i++;
                } while (i < graph.numVertices && !graph.hasEdge(i, vertex));
                return *this;
            }

            // Dereference operator (*iter)
            int operator*() const {
                return i;
            }

            // Inequality operator (iter1 != iter2)
            bool operator!=(const InEdgeIterator& other) const {
                return i != other.i;
            }
    };

    InEdgeIterator beginInEdges(int vertex) {
        int i = 0;
        while (i < numVertices && !hasEdge(i, vertex)) {
            i++;
        }
        return InEdgeIterator(*this, vertex, i);
    }

    InEdgeIterator endInEdges(int vertex) {
        return InEdgeIterator(*this, vertex, numVertices);
    }   

    // Function to retrieve the endpoints of an edge specified by an Edge_id
    pair<int, int> getEndpoints(Edge_id edge) const {
        pair<int, int> endpoints = {edge.getSource(), edge.getTarget()};
        return endpoints;
    }

    // Function to retrieve or modify the information attached to a specified edge
    int getEdgeCost(Edge_id edge) const {
        return edgeCosts.at(edge);
    }

    void setEdgeCost(Edge_id edge, int cost) {
        edgeCosts[edge] = cost;
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
    DirectedGraph graph = DirectedGraph::readGraphFromFile("graph1k.txt");
    // DirectedGraph graph = DirectedGraph::createRandomGraph(10, 20);

    // Use the graph...

    // for (DirectedGraph::VertexIterator it = graph.beginVertices(); it != graph.endVertices(); ++it) {
    //     int vertex = *it;
    //     // Do something with vertex

    //     printf("\n%d\n", vertex);
    //     // Itereate ove the out-edges of a vertex 
    //     for (DirectedGraph::OutEdgeIterator it = graph.beginOutEdges(vertex); it != graph.endOutEdges(vertex); ++it) {
    //         int targetVertex = *it;
    //         // Do something with targetVertex
    //         printf("%d->%d\n", vertex, targetVertex);
    //     }
    //     printf("\n");
    //     // Iterate over the in-edges of a vertex
    //     for (DirectedGraph::InEdgeIterator it = graph.beginInEdges(vertex); it != graph.endInEdges(vertex); ++it) {
    //         int sourceVertex = *it;
    //         // Do something with sourceVertex
    //         printf("%d->%d\n", sourceVertex, vertex);
    //     }
    // }
    
    // // Copy the graph
    // DirectedGraph graphCopy = graph;
    // // Remove an edge from the copy 
    // // graphCopy.removeEdge(0, 1);

    // // Write the graph to a file
    // // graphCopy.writeGraphToFile("graph_copy.txt");

    // // Remove a vertex from the copy
    // graphCopy.removeVertex(1);

    // // Write the graph to a file
    // graphCopy.writeGraphToFile("graph_copy.txt");
    return 0;
}
