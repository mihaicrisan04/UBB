# School Timetable Generator using Genetic Algorithms

## Project Overview

This project implements a school timetable generator using genetic algorithms (GA) with three parallel implementations:

1. **Threaded Version** - Uses C++ standard library threads and futures
2. **MPI Version** - Distributed computing using Message Passing Interface
3. **OpenCL Version** - GPU-accelerated fitness evaluation (bonus)

## Problem Description

### Timetable Scheduling Problem

The school timetable problem is a constraint satisfaction problem where we need to assign:
- **Teachers** to classes
- **Rooms** to classes
- **Timeslots** to classes

Subject to constraints:
- A teacher cannot teach multiple classes at the same time
- A room cannot host multiple classes at the same time
- A student group cannot attend multiple classes at the same time
- Teachers can only teach subjects they are qualified for
- Room capacity must accommodate the student group size

### Data Model

```cpp
struct Teacher { Id id; string name; set<Id> subjects; };
struct Room { Id id; string name; int capacity; };
struct StudentGroup { Id id; string name; int size; };
struct Subject { Id id; string name; };
struct ClassRequirement { Id id; Id subject_id; Id group_id; int hours_per_week; };
struct Assignment { Id class_req_id; Id teacher_id; Id room_id; int timeslot_idx; };
```

## Genetic Algorithm

### Chromosome Representation

A chromosome represents a complete timetable solution as a vector of `Assignment` structures. Each assignment maps a class requirement to a specific teacher, room, and timeslot.

### Fitness Function

The fitness function evaluates solution quality by counting constraint violations:

```
fitness = 1 / (1 + total_violations)
```

Where `total_violations` is the sum of:
- Teacher conflicts (same teacher at same time)
- Room conflicts (same room at same time)
- Student group conflicts (same group at same time)
- Teacher-subject incompatibility
- Room capacity violations

A fitness of 1.0 indicates a perfect solution with no violations.

### Genetic Operators

1. **Selection**: Tournament selection with configurable tournament size
2. **Crossover**: Single-point crossover with configurable rate (default 0.8)
3. **Mutation**: Random modification of teacher, room, or timeslot (default rate 0.1)
4. **Elitism**: Best individuals preserved across generations (default 2)

### Algorithm Pseudocode

```
1. Initialize random population of size N
2. Evaluate fitness of all individuals
3. For each generation:
   a. Select parents using tournament selection
   b. Apply crossover to create offspring
   c. Apply mutation to offspring
   d. Evaluate fitness of new individuals
   e. Apply elitism (keep best from previous generation)
   f. Replace population with new generation
   g. If target fitness reached, stop
4. Return best solution found
```

## Implementation Details

### 1. Threaded Version (`timetable_threaded`)

#### Parallelization Strategy

The threaded version parallelizes three main operations:

1. **Population Initialization**: Chromosomes are created in parallel using `std::async`
2. **Fitness Evaluation**: Population is divided into chunks, each evaluated by a separate thread
3. **Evolution**: Crossover and mutation operations are performed in parallel

#### Synchronization

```cpp
// Parallel fitness evaluation
void evaluate_population_parallel(std::vector<Chromosome>& population) {
    std::vector<std::future<void>> futures;
    int chunk_size = population.size() / num_threads_;

    for (int t = 0; t < num_threads_; t++) {
        futures.push_back(std::async(std::launch::async, [&, start, end]() {
            for (int i = start; i < end; i++) {
                population[i].fitness = evaluator_.evaluate(population[i]);
            }
        }));
    }

    for (auto& f : futures) f.get();  // Barrier synchronization
}
```

**Synchronization mechanisms used:**
- `std::async` with `std::future` for task-based parallelism
- `std::mutex` for protecting shared data during evolution
- Implicit barrier via `future.get()` for phase synchronization

#### Thread Safety

Each thread uses:
- Its own `GeneticOperators` instance with separate RNG
- Local storage for intermediate results
- Mutex protection only when writing to shared population

### 2. MPI Version (`timetable_mpi`)

#### Island Model Architecture

The MPI version implements an **Island Model** where:
- Each MPI process maintains its own sub-population
- Processes evolve independently
- Periodic migration exchanges best individuals between islands

```
Process 0 <--> Process 1 <--> Process 2 <--> ... <--> Process N-1
    ^                                                      |
    |______________________________________________________|
                    (Ring topology)
```

#### Parallelization Strategy

1. **Population Distribution**: Total population divided among processes
2. **Independent Evolution**: Each process evolves its local population
3. **Migration**: Best individuals exchanged using ring topology
4. **Global Best**: Periodically gathered using collective operations

#### Synchronization and Communication

```cpp
// Ring topology migration
void migrate(std::vector<Chromosome>& population) {
    int dest = (rank_ + 1) % size_;
    int src = (rank_ - 1 + size_) % size_;

    // Serialize best individuals
    ChromosomeSerializer::serialize(to_send, send_buffer);

    // Exchange with neighbors
    MPI_Sendrecv(send_buffer.data(), size, MPI_CHAR, dest, 0,
                 recv_buffer.data(), size, MPI_CHAR, src, 0,
                 comm_, MPI_STATUS_IGNORE);

    // Integrate received individuals
    ...
}
```

**MPI operations used:**
- `MPI_Sendrecv` for migration between neighbors
- `MPI_Gather` for collecting local bests to root
- `MPI_Bcast` for distributing global best
- `MPI_Barrier` for synchronization points

#### Chromosome Serialization

Custom serialization for MPI communication:
```cpp
class ChromosomeSerializer {
    static void serialize(const Chromosome& c, std::vector<char>& buffer);
    static Chromosome deserialize(const char* buffer);
};
```

### 3. OpenCL Version (`timetable_opencl`)

#### GPU Acceleration Strategy

The OpenCL version accelerates **fitness evaluation** on the GPU:
- Chromosomes are evaluated in parallel (one work-item per chromosome)
- Problem data (teachers, rooms, requirements) stored in GPU memory
- Evolution operations remain on CPU

#### Kernel Design

```opencl
__kernel void evaluate_fitness(
    __global const Assignment* assignments,
    __global const int* assignment_offsets,
    __global const int* assignment_counts,
    __global const ClassRequirement* requirements,
    // ... other problem data
    __global double* fitness_out)
{
    int gid = get_global_id(0);  // Chromosome index
    // Count violations and compute fitness
    fitness_out[gid] = 1.0 / (1.0 + violations);
}
```

#### Memory Management

- Problem data buffers created once at initialization
- Per-generation buffers for assignments created and released each iteration
- Data transfer: Host -> Device (assignments), Device -> Host (fitness)

#### Synchronization

```cpp
// Synchronous execution
clEnqueueNDRangeKernel(queue_, kernel_, 1, nullptr, &global_size, nullptr, 0, nullptr, nullptr);
clEnqueueReadBuffer(queue_, fitness_buf, CL_TRUE, ...);  // Blocking read
```

## Performance Analysis

### Theoretical Complexity

| Operation | Sequential | Threaded (P threads) | MPI (N processes) |
|-----------|------------|---------------------|-------------------|
| Initialization | O(pop × genes) | O(pop × genes / P) | O(pop × genes / N) |
| Fitness Eval | O(pop × genes²) | O(pop × genes² / P) | O(pop × genes² / N) |
| Selection | O(pop × tournament) | O(pop × tournament / P) | O(pop × tournament / N) |
| Crossover | O(pop × genes) | O(pop × genes / P) | O(pop × genes / N) |
| Migration (MPI) | - | - | O(migration_count × gene_size) |

### Expected Speedup

**Threaded Version:**
- Near-linear speedup up to physical core count
- Limited by Amdahl's Law (sequential portions)
- Overhead from thread creation and synchronization

**MPI Version:**
- Good scaling for large populations
- Migration overhead depends on interval and count
- Communication latency more significant than compute

**OpenCL Version:**
- Best for large populations with expensive fitness evaluation
- Data transfer overhead may dominate for small problems
- GPU occupancy depends on population size

### Benchmark Results Template

Run benchmarks with:
```bash
# Threaded benchmark
./bin/timetable_threaded --benchmark --pop 200 --gen 100

# MPI scaling test
for np in 1 2 4 8; do
    mpirun -np $np ./bin/timetable_mpi --pop 200 --gen 100 --quiet
done

# OpenCL vs CPU comparison
./bin/timetable_opencl --pop 500 --gen 100
./bin/timetable_threaded --pop 500 --gen 100 --threads 1
```

## Building and Running

### Prerequisites

- CMake 3.14+
- C++17 compiler (clang++ or g++)
- MPI implementation (open-mpi)
- OpenCL framework (included in macOS)

### Build Instructions

```bash
cd timetable-ga
mkdir build && cd build
cmake ..
make -j$(nproc)
```

### Running

```bash
# Threaded version
./bin/timetable_threaded --threads 8 --pop 100 --gen 500

# MPI version
mpirun -np 4 ./bin/timetable_mpi --pop 100 --gen 500

# OpenCL version
./bin/timetable_opencl --pop 100 --gen 500
```

### Command-line Options

| Option | Description | Default |
|--------|-------------|---------|
| `--threads N` | Number of threads (threaded only) | Hardware concurrency |
| `--pop N` | Population size | 100 |
| `--gen N` | Maximum generations | 500 |
| `--teachers N` | Number of teachers | 10 |
| `--rooms N` | Number of rooms | 8 |
| `--groups N` | Number of student groups | 6 |
| `--subjects N` | Number of subjects | 8 |
| `--classes N` | Classes per group | 5 |
| `--small` | Use small test problem | - |
| `--quiet` | Minimal output | - |
| `--benchmark` | Run benchmark (threaded only) | - |
| `--migration N` | Migration interval (MPI only) | 50 |

## File Structure

```
timetable-ga/
├── CMakeLists.txt
├── DOCS.md
├── README.md
└── src/
    ├── common/
    │   ├── types.hpp           # Data structures
    │   ├── fitness.hpp         # Fitness evaluation
    │   ├── operators.hpp       # Genetic operators
    │   ├── problem_generator.hpp # Random problem generation
    │   └── utils.hpp           # Utilities and timing
    ├── threaded/
    │   ├── ga_threaded.hpp     # Threaded GA implementation
    │   └── main.cpp
    ├── mpi/
    │   ├── ga_mpi.hpp          # MPI GA implementation
    │   └── main.cpp
    └── opencl/
        ├── ga_opencl.hpp       # OpenCL GA implementation
        ├── kernels.cl          # OpenCL kernels
        └── main.cpp
```

## References

1. Holland, J. H. (1975). Adaptation in Natural and Artificial Systems
2. Goldberg, D. E. (1989). Genetic Algorithms in Search, Optimization, and Machine Learning
3. Burke, E. K., & Petrovic, S. (2002). Recent research directions in automated timetabling
