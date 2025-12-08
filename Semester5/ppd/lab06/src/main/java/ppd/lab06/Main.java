package ppd.lab06;

import java.util.*;

public class Main {
    public static void main(String[] args) {
        int n = 10, threads = 8;
        
        // Graph WITH Hamiltonian cycle
        Graph withCycle = Graph.createWithHamiltonianCycle(n, 10, new Random(42));
        
        // Graph WITHOUT Hamiltonian cycle (star graph - no cycle possible)
        Graph noCycle = new Graph(n);
        for (int i = 1; i < n; i++) noCycle.addEdge(0, i); // hub -> all
        for (int i = 1; i < n; i++) noCycle.addEdge(i, 0); // all -> hub
        
        System.out.println("=== Graph WITH Hamiltonian Cycle ===");
        run(withCycle, threads);
        
        System.out.println("\n=== Graph WITHOUT Hamiltonian Cycle ===");
        run(noCycle, threads);
    }
    
    static void run(Graph g, int threads) {
        var seq = new SequentialHamiltonian(g, 0).findCycle();
        var par = new ParallelHamiltonian(g, 0, threads).findCycle();
        var fj = new ForkJoinHamiltonian(g, 0, threads).findCycle();
        
        System.out.printf("Sequential:  %s (%.2fms)%n", seq.isFound() ? seq.getCycle() : "none", seq.getExecutionTimeMs());
        System.out.printf("Parallel:    %s (%.2fms)%n", par.isFound() ? par.getCycle() : "none", par.getExecutionTimeMs());
        System.out.printf("ForkJoin:    %s (%.2fms)%n", fj.isFound() ? fj.getCycle() : "none", fj.getExecutionTimeMs());
    }
}
