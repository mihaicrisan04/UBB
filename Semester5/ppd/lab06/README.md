# Lab 06 - Parallel Hamiltonian Cycle Search

## Goal

Find a Hamiltonian cycle in a directed graph using parallel algorithms.

## Problem Description

Given a directed graph, find a Hamiltonian cycle (a cycle that visits every vertex exactly once and returns to the starting vertex), if one exists. The search uses multiple threads to parallelize the exploration of different branches.

## Algorithms

### 1. Sequential Backtracking (Baseline)

The sequential algorithm uses depth-first search with backtracking:

1. Start from a fixed vertex (vertex 0)
2. Mark the current vertex as visited and add to path
3. For each unvisited neighbor:
   - Recursively explore that branch
   - If a cycle is found, return it
   - Otherwise, backtrack (unmark vertex, remove from path)
4. When all vertices are visited, check if there's an edge back to the start

**Time Complexity**: O(n!) in the worst case, where n is the number of vertices.

### 2. Parallel with Manual Thread Allocation

This implementation distributes threads across branches based on the out-degree of vertices at each level.

**Thread Distribution Strategy**:
- At each vertex with k unvisited neighbors and t available threads:
  - Each branch gets `t/k` threads (base allocation)
  - The first `t mod k` branches get one extra thread
- Example: 8 threads, 3 branches → [3, 3, 2] threads per branch

**Algorithm**:
1. Start with all threads at the root (start vertex)
2. At each vertex, distribute available threads across unvisited neighbors
3. When a branch has only 1 thread, switch to sequential search
4. Use `AtomicBoolean` to signal all threads to stop once a cycle is found
5. Use `AtomicReference` to safely store the found cycle

**Synchronization**:
- `AtomicBoolean shouldStop`: Early termination flag
- `AtomicReference<List<Integer>> foundCycle`: Thread-safe result storage
- Each thread works on its own copy of visited[] and path (no shared mutable state during search)
- `ExecutorService` for thread management at each fork point

### 3. ForkJoinPool with RecursiveTask

This implementation uses Java's Fork/Join framework designed for divide-and-conquer parallelism.

**Algorithm**:
1. Create a `ForkJoinPool` with the specified parallelism
2. Submit an initial `HamiltonianTask` starting from the start vertex
3. Each task:
   - If at shallow depth (< threshold) and multiple neighbors: **fork** subtasks
   - If at deeper depth or single neighbor: continue **sequentially**
4. Use work-stealing to balance load across threads

**Key Design Decisions**:
- **Parallel threshold**: Controlled by `parallelThreshold = log(threads) + 2`
  - Fork only at shallow depths to avoid excessive task creation overhead
  - Deeper exploration is done sequentially within each task
- **Last-task optimization**: The last branch is computed directly (not forked) to reduce overhead

**Synchronization**:
- `AtomicBoolean found`: Signals when a cycle is found
- `AtomicReference<List<Integer>> result`: Stores the found cycle
- Fork/Join framework handles thread scheduling and work-stealing

## Performance Characteristics

| Aspect | Sequential | Parallel (Manual) | ForkJoinPool |
|--------|------------|-------------------|--------------|
| Overhead | None | Thread creation at each level | Task creation overhead |
| Load Balancing | N/A | Static (predetermined) | Dynamic (work-stealing) |
| Memory | O(n) | O(n × branches) per level | O(n × tasks) |
| Termination | Immediate | Atomic flag check | Atomic flag check |

**When to expect speedup**:
- Large graphs with many vertices
- Dense graphs with high out-degree (more branches to explore in parallel)
- When the Hamiltonian cycle is "far" in the search space

**When parallelization may not help**:
- Small graphs (overhead dominates)
- Sparse graphs (limited parallelism opportunities)
- When the cycle is found early in the search

## Project Structure

```
lab06/
├── pom.xml                                    # Maven build configuration
├── README.md                                  # This documentation
└── src/main/java/ppd/lab06/
    ├── Main.java                              # Entry point with benchmarking
    ├── Graph.java                             # Directed graph representation
    ├── HamiltonianResult.java                 # Search result container
    ├── SequentialHamiltonian.java             # Sequential backtracking
    ├── ParallelHamiltonian.java               # Manual thread distribution
    └── ForkJoinHamiltonian.java               # ForkJoinPool implementation
```

## Building and Running

### With Maven

```bash
# Compile
mvn compile

# Package into JAR
mvn package

# Run
java -jar target/lab06.jar
```

### Direct compilation

```bash
# Compile
javac -d target/classes src/main/java/ppd/lab06/*.java

# Run
java -cp target/classes ppd.lab06.Main
```

## Sample Output

```
═══════════════════════════════════════════════════════════════════
           Hamiltonian Cycle Search - Parallel Algorithms          
═══════════════════════════════════════════════════════════════════

───────────────────────────────────────────────────────────────────
Graph: 14 vertices, 25 extra edges, 8 threads
───────────────────────────────────────────────────────────────────
Warming up...

Sequential:                       12.45 ms
Parallel (manual threads):         4.21 ms (speedup: 2.96x)
ForkJoinPool:                      3.87 ms (speedup: 3.22x)
```

## Performance Measurements

The program includes built-in benchmarking that:
1. Creates graphs with guaranteed Hamiltonian cycles
2. Runs warmup iterations (JIT compilation)
3. Averages results over multiple runs
4. Reports execution time and speedup factor

Typical observations:
- ForkJoinPool often performs slightly better due to work-stealing
- Speedup improves with larger graphs and more threads
- The overhead of thread creation can dominate for small graphs

## Implementation Notes

### Thread Safety

Both parallel implementations ensure thread safety through:
1. **Immutable sharing**: Each branch gets its own copy of visited[] and path
2. **Atomic operations**: `compareAndSet` for result updates
3. **Early termination**: Atomic flag checked before each recursive call

### Graph Generation

The `Graph.createWithHamiltonianCycle()` method:
1. Creates a random permutation of vertices
2. Adds edges to form a cycle through this permutation
3. Adds random extra edges for increased complexity

This guarantees a Hamiltonian cycle exists while creating a non-trivial search space.
