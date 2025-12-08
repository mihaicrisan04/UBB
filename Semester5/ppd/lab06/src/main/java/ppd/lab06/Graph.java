package ppd.lab06;

import java.util.*;

/**
 * Represents a directed graph using an adjacency list.
 */
public class Graph {
    private final int vertices;
    private final List<List<Integer>> adjacencyList;
    
    public Graph(int vertices) {
        this.vertices = vertices;
        this.adjacencyList = new ArrayList<>(vertices);
        for (int i = 0; i < vertices; i++) {
            adjacencyList.add(new ArrayList<>());
        }
    }
    
    public void addEdge(int from, int to) {
        if (from >= 0 && from < vertices && to >= 0 && to < vertices) {
            adjacencyList.get(from).add(to);
        }
    }
    
    public List<Integer> getNeighbors(int vertex) {
        return adjacencyList.get(vertex);
    }
    
    public int getVertexCount() {
        return vertices;
    }
    
    public int getOutDegree(int vertex) {
        return adjacencyList.get(vertex).size();
    }
    
    /**
     * Creates a random directed graph with a guaranteed Hamiltonian cycle.
     * First creates a cycle, then adds random edges.
     */
    public static Graph createWithHamiltonianCycle(int vertices, int extraEdges, Random random) {
        Graph graph = new Graph(vertices);
        
        // Create a random permutation for the Hamiltonian cycle
        List<Integer> cycle = new ArrayList<>();
        for (int i = 0; i < vertices; i++) {
            cycle.add(i);
        }
        Collections.shuffle(cycle, random);
        
        // Add edges for the Hamiltonian cycle
        for (int i = 0; i < vertices; i++) {
            int from = cycle.get(i);
            int to = cycle.get((i + 1) % vertices);
            graph.addEdge(from, to);
        }
        
        // Add extra random edges
        for (int i = 0; i < extraEdges; i++) {
            int from = random.nextInt(vertices);
            int to = random.nextInt(vertices);
            if (from != to && !graph.adjacencyList.get(from).contains(to)) {
                graph.addEdge(from, to);
            }
        }
        
        return graph;
    }
    
    /**
     * Creates a random directed graph (may or may not have Hamiltonian cycle).
     */
    public static Graph createRandom(int vertices, double edgeProbability, Random random) {
        Graph graph = new Graph(vertices);
        
        for (int i = 0; i < vertices; i++) {
            for (int j = 0; j < vertices; j++) {
                if (i != j && random.nextDouble() < edgeProbability) {
                    graph.addEdge(i, j);
                }
            }
        }
        
        return graph;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Graph with ").append(vertices).append(" vertices:\n");
        for (int i = 0; i < vertices; i++) {
            sb.append(i).append(" -> ").append(adjacencyList.get(i)).append("\n");
        }
        return sb.toString();
    }
}

