package ppd.lab06;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

/**
 * Parallel implementation of Hamiltonian cycle search using manual thread allocation.
 * 
 * Threads are distributed across branches at multiple levels based on the out-degree
 * of each vertex. For example, with 8 threads and a vertex with 3 neighbors:
 * - Two branches get 3 threads each
 * - One branch gets 2 threads
 * 
 * When a branch has only 1 thread allocated, it performs sequential search.
 */
public class ParallelHamiltonian {
    private final Graph graph;
    private final int startVertex;
    private final int numThreads;
    
    // Shared result - once found, all threads should stop
    private final AtomicReference<List<Integer>> foundCycle = new AtomicReference<>(null);
    private final AtomicBoolean shouldStop = new AtomicBoolean(false);
    
    public ParallelHamiltonian(Graph graph, int startVertex, int numThreads) {
        this.graph = graph;
        this.startVertex = startVertex;
        this.numThreads = numThreads;
    }
    
    public HamiltonianResult findCycle() {
        long startTime = System.nanoTime();
        
        int n = graph.getVertexCount();
        if (n == 0) {
            return new HamiltonianResult(false, null, System.nanoTime() - startTime);
        }
        
        // Reset state for fresh search
        foundCycle.set(null);
        shouldStop.set(false);
        
        boolean[] visited = new boolean[n];
        List<Integer> path = new ArrayList<>();
        
        path.add(startVertex);
        visited[startVertex] = true;
        
        // Start parallel search with all threads
        parallelBacktrack(visited, path, numThreads);
        
        long endTime = System.nanoTime();
        List<Integer> result = foundCycle.get();
        
        if (result != null) {
            return new HamiltonianResult(true, result, endTime - startTime);
        }
        return new HamiltonianResult(false, null, endTime - startTime);
    }
    
    /**
     * Performs backtracking with the given number of threads available.
     * Distributes threads across branches based on out-degree.
     */
    private void parallelBacktrack(boolean[] visited, List<Integer> path, int availableThreads) {
        if (shouldStop.get()) {
            return;
        }
        
        int n = graph.getVertexCount();
        
        // Check if we have a complete path
        if (path.size() == n) {
            int lastVertex = path.get(path.size() - 1);
            List<Integer> neighbors = graph.getNeighbors(lastVertex);
            
            if (neighbors.contains(startVertex)) {
                // Found a Hamiltonian cycle
                List<Integer> cycle = new ArrayList<>(path);
                cycle.add(startVertex);
                
                // Atomically set the result (only first finder wins)
                if (foundCycle.compareAndSet(null, cycle)) {
                    shouldStop.set(true);
                }
            }
            return;
        }
        
        int currentVertex = path.get(path.size() - 1);
        List<Integer> neighbors = graph.getNeighbors(currentVertex);
        
        // Filter unvisited neighbors
        List<Integer> unvisitedNeighbors = new ArrayList<>();
        for (int neighbor : neighbors) {
            if (!visited[neighbor]) {
                unvisitedNeighbors.add(neighbor);
            }
        }
        
        if (unvisitedNeighbors.isEmpty()) {
            return;
        }
        
        // If only one thread available or only one neighbor, do sequential
        if (availableThreads <= 1 || unvisitedNeighbors.size() == 1) {
            sequentialBacktrack(visited, path, unvisitedNeighbors);
            return;
        }
        
        // Distribute threads across branches
        int[] threadDistribution = distributeThreads(availableThreads, unvisitedNeighbors.size());
        
        // Create tasks for parallel execution
        List<Future<?>> futures = new ArrayList<>();
        ExecutorService executor = Executors.newFixedThreadPool(
            Math.min(availableThreads, unvisitedNeighbors.size())
        );
        
        try {
            for (int i = 0; i < unvisitedNeighbors.size(); i++) {
                if (shouldStop.get()) {
                    break;
                }
                
                final int neighbor = unvisitedNeighbors.get(i);
                final int threadsForBranch = threadDistribution[i];
                
                // Create a copy of visited and path for this branch
                final boolean[] branchVisited = visited.clone();
                final List<Integer> branchPath = new ArrayList<>(path);
                
                branchVisited[neighbor] = true;
                branchPath.add(neighbor);
                
                futures.add(executor.submit(() -> {
                    if (threadsForBranch > 1) {
                        parallelBacktrack(branchVisited, branchPath, threadsForBranch);
                    } else {
                        // Single thread - do sequential from here
                        sequentialFromState(branchVisited, branchPath);
                    }
                }));
            }
            
            // Wait for all tasks to complete
            for (Future<?> future : futures) {
                try {
                    future.get();
                } catch (InterruptedException | ExecutionException e) {
                    Thread.currentThread().interrupt();
                }
            }
        } finally {
            executor.shutdown();
        }
    }
    
    /**
     * Distributes threads across branches as evenly as possible.
     * Example: 8 threads, 3 branches -> [3, 3, 2]
     */
    private int[] distributeThreads(int threads, int branches) {
        int[] distribution = new int[branches];
        int base = threads / branches;
        int remainder = threads % branches;
        
        for (int i = 0; i < branches; i++) {
            distribution[i] = base + (i < remainder ? 1 : 0);
        }
        
        return distribution;
    }
    
    /**
     * Sequential backtracking through a list of neighbors.
     */
    private void sequentialBacktrack(boolean[] visited, List<Integer> path, List<Integer> neighbors) {
        for (int neighbor : neighbors) {
            if (shouldStop.get()) {
                return;
            }
            
            visited[neighbor] = true;
            path.add(neighbor);
            
            sequentialFromState(visited, path);
            
            // Backtrack
            path.remove(path.size() - 1);
            visited[neighbor] = false;
        }
    }
    
    /**
     * Continues sequential search from a given state.
     */
    private void sequentialFromState(boolean[] visited, List<Integer> path) {
        if (shouldStop.get()) {
            return;
        }
        
        int n = graph.getVertexCount();
        
        if (path.size() == n) {
            int lastVertex = path.get(path.size() - 1);
            List<Integer> neighbors = graph.getNeighbors(lastVertex);
            
            if (neighbors.contains(startVertex)) {
                List<Integer> cycle = new ArrayList<>(path);
                cycle.add(startVertex);
                
                if (foundCycle.compareAndSet(null, cycle)) {
                    shouldStop.set(true);
                }
            }
            return;
        }
        
        int currentVertex = path.get(path.size() - 1);
        List<Integer> neighbors = graph.getNeighbors(currentVertex);
        
        for (int neighbor : neighbors) {
            if (shouldStop.get()) {
                return;
            }
            
            if (!visited[neighbor]) {
                visited[neighbor] = true;
                path.add(neighbor);
                
                sequentialFromState(visited, path);
                
                // Backtrack
                path.remove(path.size() - 1);
                visited[neighbor] = false;
            }
        }
    }
}

