package ppd.lab06;

import java.util.List;

/**
 * Represents the result of a Hamiltonian cycle search.
 */
public class HamiltonianResult {
    private final boolean found;
    private final List<Integer> cycle;
    private final long executionTimeNanos;
    
    public HamiltonianResult(boolean found, List<Integer> cycle, long executionTimeNanos) {
        this.found = found;
        this.cycle = cycle;
        this.executionTimeNanos = executionTimeNanos;
    }
    
    public boolean isFound() {
        return found;
    }
    
    public List<Integer> getCycle() {
        return cycle;
    }
    
    public long getExecutionTimeNanos() {
        return executionTimeNanos;
    }
    
    public double getExecutionTimeMs() {
        return executionTimeNanos / 1_000_000.0;
    }
    
    @Override
    public String toString() {
        if (found) {
            return String.format("Found Hamiltonian cycle: %s (%.2f ms)", cycle, getExecutionTimeMs());
        } else {
            return String.format("No Hamiltonian cycle found (%.2f ms)", getExecutionTimeMs());
        }
    }
}

