package ppd.lab06;

import java.util.*;

/**
 * Sequential (single-threaded) implementation of Hamiltonian cycle search.
 * Uses backtracking with DFS.
 */
public class SequentialHamiltonian {
    private final Graph graph;
    private final int startVertex;
    
    public SequentialHamiltonian(Graph graph, int startVertex) {
        this.graph = graph;
        this.startVertex = startVertex;
    }
    
    public HamiltonianResult findCycle() {
        long startTime = System.nanoTime();
        
        int n = graph.getVertexCount();
        if (n == 0) {
            return new HamiltonianResult(false, null, System.nanoTime() - startTime);
        }
        
        boolean[] visited = new boolean[n];
        List<Integer> path = new ArrayList<>();
        
        path.add(startVertex);
        visited[startVertex] = true;
        
        List<Integer> result = backtrack(visited, path);
        
        long endTime = System.nanoTime();
        
        if (result != null) {
            return new HamiltonianResult(true, result, endTime - startTime);
        }
        return new HamiltonianResult(false, null, endTime - startTime);
    }
    
    private List<Integer> backtrack(boolean[] visited, List<Integer> path) {
        int n = graph.getVertexCount();
        
        // If we've visited all vertices, check if we can return to start
        if (path.size() == n) {
            int lastVertex = path.get(path.size() - 1);
            List<Integer> neighbors = graph.getNeighbors(lastVertex);
            
            if (neighbors.contains(startVertex)) {
                // Found a Hamiltonian cycle
                List<Integer> cycle = new ArrayList<>(path);
                cycle.add(startVertex);
                return cycle;
            }
            return null;
        }
        
        int currentVertex = path.get(path.size() - 1);
        List<Integer> neighbors = graph.getNeighbors(currentVertex);
        
        for (int neighbor : neighbors) {
            if (!visited[neighbor]) {
                visited[neighbor] = true;
                path.add(neighbor);
                
                List<Integer> result = backtrack(visited, path);
                if (result != null) {
                    return result;
                }
                
                // Backtrack
                path.remove(path.size() - 1);
                visited[neighbor] = false;
            }
        }
        
        return null;
    }
}

