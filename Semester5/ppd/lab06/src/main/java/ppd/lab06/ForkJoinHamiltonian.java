package ppd.lab06;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

/**
 * Parallel implementation of Hamiltonian cycle search using ForkJoinPool and RecursiveTask.
 * 
 * Each task explores one branch of the search tree. Tasks are forked for each
 * unvisited neighbor, and the search terminates early when a cycle is found.
 */
public class ForkJoinHamiltonian {
    private final Graph graph;
    private final int startVertex;
    private final int parallelism;
    
    // Shared flag to signal all tasks to stop once a cycle is found
    private final AtomicBoolean found = new AtomicBoolean(false);
    private final AtomicReference<List<Integer>> result = new AtomicReference<>(null);
    
    // Threshold: below this path length, we parallelize; above we go sequential
    // This can be tuned based on the graph size
    private final int parallelThreshold;
    
    public ForkJoinHamiltonian(Graph graph, int startVertex, int parallelism) {
        this.graph = graph;
        this.startVertex = startVertex;
        this.parallelism = parallelism;
        // Parallelize until we've made a few choices, then go sequential
        this.parallelThreshold = Math.max(2, (int) Math.log(parallelism) + 2);
    }
    
    public HamiltonianResult findCycle() {
        long startTime = System.nanoTime();
        
        int n = graph.getVertexCount();
        if (n == 0) {
            return new HamiltonianResult(false, null, System.nanoTime() - startTime);
        }
        
        // Reset state
        found.set(false);
        result.set(null);
        
        ForkJoinPool pool = new ForkJoinPool(parallelism);
        
        try {
            boolean[] initialVisited = new boolean[n];
            List<Integer> initialPath = new ArrayList<>();
            initialPath.add(startVertex);
            initialVisited[startVertex] = true;
            
            HamiltonianTask task = new HamiltonianTask(initialVisited, initialPath, 0);
            pool.invoke(task);
        } finally {
            pool.shutdown();
        }
        
        long endTime = System.nanoTime();
        List<Integer> cycle = result.get();
        
        if (cycle != null) {
            return new HamiltonianResult(true, cycle, endTime - startTime);
        }
        return new HamiltonianResult(false, null, endTime - startTime);
    }
    
    /**
     * RecursiveTask that searches for a Hamiltonian cycle.
     * Returns true if a cycle was found in this branch or any sub-branch.
     */
    private class HamiltonianTask extends RecursiveTask<Boolean> {
        private final boolean[] visited;
        private final List<Integer> path;
        private final int depth;
        
        HamiltonianTask(boolean[] visited, List<Integer> path, int depth) {
            this.visited = visited;
            this.path = path;
            this.depth = depth;
        }
        
        @Override
        protected Boolean compute() {
            // Early termination if another task found a cycle
            if (found.get()) {
                return false;
            }
            
            int n = graph.getVertexCount();
            
            // Check if we have a complete path
            if (path.size() == n) {
                int lastVertex = path.get(path.size() - 1);
                List<Integer> neighbors = graph.getNeighbors(lastVertex);
                
                if (neighbors.contains(startVertex)) {
                    // Found a Hamiltonian cycle!
                    List<Integer> cycle = new ArrayList<>(path);
                    cycle.add(startVertex);
                    
                    if (found.compareAndSet(false, true)) {
                        result.set(cycle);
                    }
                    return true;
                }
                return false;
            }
            
            int currentVertex = path.get(path.size() - 1);
            List<Integer> neighbors = graph.getNeighbors(currentVertex);
            
            // Collect unvisited neighbors
            List<Integer> unvisitedNeighbors = new ArrayList<>();
            for (int neighbor : neighbors) {
                if (!visited[neighbor]) {
                    unvisitedNeighbors.add(neighbor);
                }
            }
            
            if (unvisitedNeighbors.isEmpty()) {
                return false;
            }
            
            // Decide whether to fork or compute sequentially
            // Fork if we're still at a shallow depth and have multiple choices
            boolean shouldFork = depth < parallelThreshold && unvisitedNeighbors.size() > 1;
            
            if (shouldFork) {
                return computeParallel(unvisitedNeighbors);
            } else {
                return computeSequential(unvisitedNeighbors);
            }
        }
        
        /**
         * Fork subtasks for each unvisited neighbor.
         */
        private boolean computeParallel(List<Integer> unvisitedNeighbors) {
            List<HamiltonianTask> tasks = new ArrayList<>();
            
            // Fork tasks for all but one neighbor
            for (int i = 0; i < unvisitedNeighbors.size() - 1; i++) {
                if (found.get()) {
                    break;
                }
                
                int neighbor = unvisitedNeighbors.get(i);
                
                // Create copies for the forked task
                boolean[] newVisited = visited.clone();
                List<Integer> newPath = new ArrayList<>(path);
                
                newVisited[neighbor] = true;
                newPath.add(neighbor);
                
                HamiltonianTask task = new HamiltonianTask(newVisited, newPath, depth + 1);
                task.fork();
                tasks.add(task);
            }
            
            // Compute the last branch in this thread (optimization)
            boolean foundInLast = false;
            if (!found.get() && !unvisitedNeighbors.isEmpty()) {
                int lastNeighbor = unvisitedNeighbors.get(unvisitedNeighbors.size() - 1);
                
                boolean[] newVisited = visited.clone();
                List<Integer> newPath = new ArrayList<>(path);
                
                newVisited[lastNeighbor] = true;
                newPath.add(lastNeighbor);
                
                HamiltonianTask lastTask = new HamiltonianTask(newVisited, newPath, depth + 1);
                foundInLast = lastTask.compute();
            }
            
            // Join forked tasks
            boolean foundInForked = false;
            for (HamiltonianTask task : tasks) {
                if (task.join()) {
                    foundInForked = true;
                }
            }
            
            return foundInLast || foundInForked;
        }
        
        /**
         * Sequential backtracking for deeper levels.
         */
        private boolean computeSequential(List<Integer> unvisitedNeighbors) {
            for (int neighbor : unvisitedNeighbors) {
                if (found.get()) {
                    return false;
                }
                
                visited[neighbor] = true;
                path.add(neighbor);
                
                // Recursive sequential search
                if (sequentialSearch()) {
                    return true;
                }
                
                // Backtrack
                path.remove(path.size() - 1);
                visited[neighbor] = false;
            }
            
            return false;
        }
        
        /**
         * Pure sequential search for deeper levels.
         */
        private boolean sequentialSearch() {
            if (found.get()) {
                return false;
            }
            
            int n = graph.getVertexCount();
            
            if (path.size() == n) {
                int lastVertex = path.get(path.size() - 1);
                List<Integer> neighbors = graph.getNeighbors(lastVertex);
                
                if (neighbors.contains(startVertex)) {
                    List<Integer> cycle = new ArrayList<>(path);
                    cycle.add(startVertex);
                    
                    if (found.compareAndSet(false, true)) {
                        result.set(cycle);
                    }
                    return true;
                }
                return false;
            }
            
            int currentVertex = path.get(path.size() - 1);
            List<Integer> neighbors = graph.getNeighbors(currentVertex);
            
            for (int neighbor : neighbors) {
                if (found.get()) {
                    return false;
                }
                
                if (!visited[neighbor]) {
                    visited[neighbor] = true;
                    path.add(neighbor);
                    
                    if (sequentialSearch()) {
                        return true;
                    }
                    
                    path.remove(path.size() - 1);
                    visited[neighbor] = false;
                }
            }
            
            return false;
        }
    }
}

