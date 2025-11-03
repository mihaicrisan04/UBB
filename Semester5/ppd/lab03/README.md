# Lab 03: Matrix Multiplication with Threading (Java)

## Problem Statement

Divide matrix multiplication between threads using different task distribution strategies to observe caching effects and threading overhead.

## Implementation

The program computes the product of two matrices A(NxM) and B(MxP) to produce C(NxP) using three different threading strategies.

### Classes

- **MatrixMultiply.java**: Main class implementing all three strategies
- **Benchmark.java**: Comprehensive performance testing suite

### Core Components

1. **computeElement(row, col, threadId)**: Computes a single element of the result matrix
2. **Inner Runnable classes**: Each implements a different distribution strategy

### Distribution Strategies

#### Strategy 1: Row-by-row consecutive elements
Each thread computes consecutive elements going row after row.

**Example** (9x9 matrix, 4 threads, 81 total elements):
- Thread 0: rows 0-1, elements 0-1 of row 2 (20 elements)
- Thread 1: remainder of row 2, row 3, elements 0-3 of row 4 (20 elements)
- Thread 2: remainder of row 4, row 5, elements 0-5 of row 6 (20 elements)
- Thread 3: remaining elements (21 elements)

#### Strategy 2: Column-by-column consecutive elements
Each thread computes consecutive elements going column after column.

**Example** (9x9 matrix, 4 threads):
- Thread 0: columns 0-1, elements 0-1 from column 2
- Thread 1: remainder of column 2, column 3, elements 0-3 of column 4
- And so on...

#### Strategy 3: Every k-th element
Each thread takes every k-th element (k = number of threads), going row by row.

**Example** (9x9 matrix, 4 threads):
- Thread 0: elements at indices 0, 4, 8, 12, 16, 20, 24, ... (every 4th starting from 0)
- Thread 1: elements at indices 1, 5, 9, 13, 17, 21, 25, ... (every 4th starting from 1)
- Thread 2: elements at indices 2, 6, 10, 14, 18, 22, 26, ... (every 4th starting from 2)
- Thread 3: elements at indices 3, 7, 11, 15, 19, 23, 27, ... (every 4th starting from 3)

---

## Setup & Running

### Prerequisites

Make sure you have Java installed and jenv configured (if using jenv).

Check your Java version:
```bash
java -version
```

If using jenv, set your preferred Java version for this directory:
```bash
jenv local 21  # or whatever version you want
```

---

## Option 1: Using Make (Recommended)

### Step 1: Navigate to lab03
```bash
cd ppd/lab03
```

### Step 2: Set Java version (if using jenv)
```bash
jenv local 21  # or your preferred version
jenv version   # verify it's set correctly
```

### Step 3: Compile everything
```bash
make
```
This compiles both `MatrixMultiply.java` and `Benchmark.java`.

### Step 4: Run different targets

**Basic run** (1000x1000 matrix, 4 threads):
```bash
make run
```

**Debug mode** (9x9 matrix, shows which thread computes each element):
```bash
make debug
```

**Full benchmark suite** (tests various sizes and thread counts):
```bash
make benchmark
```

**Clean compiled files**:
```bash
make clean
```

---

## Option 2: Direct Java Commands (No Make)

### Step 1: Navigate to lab03
```bash
cd ppd/lab03
```

### Step 2: Set Java version (if using jenv)
```bash
jenv local 21  # or your preferred version
jenv version   # verify it's set correctly
```

### Step 3: Compile the Java files
```bash
javac MatrixMultiply.java
javac Benchmark.java
```

Or compile both at once:
```bash
javac MatrixMultiply.java Benchmark.java
```

### Step 4: Run the programs

**Basic run** with custom parameters:
```bash
java MatrixMultiply <rows_A> <cols_A> <cols_B> <num_threads> [debug]
```

Examples:
```bash
# 1000x1000 matrices, 4 threads
java MatrixMultiply 1000 1000 1000 4

# 500x800 * 800x1000, 8 threads
java MatrixMultiply 500 800 1000 8

# Small matrix with debug output
java MatrixMultiply 9 9 9 4 debug
```

**Run benchmark suite**:
```bash
java Benchmark
```

**Clean up**:
```bash
rm *.class
```

---

## Example Outputs

### Debug Mode (Small Matrix)
```
Matrix multiplication: A(9x9) * B(9x9) = C(9x9)
Number of threads: 4

Strategy 1: Row-by-row consecutive elements
Thread 0 computing element (0, 0)
Thread 0 computing element (0, 1)
Thread 1 computing element (2, 3)
Thread 2 computing element (4, 6)
...
  Time: 0.52 ms
✓ Result verified successfully
```

### Performance Run
```
Matrix multiplication: A(1000x1000) * B(1000x1000) = C(1000x1000)
Number of threads: 4

Strategy 1: Row-by-row consecutive elements
  Time: 1234.56 ms

Strategy 2: Column-by-column consecutive elements
  Time: 2345.67 ms

Strategy 3: Every k-th element
  Time: 3456.78 ms
```

---

## Performance Observations

### Expected Results

**Thread count impact:**
- 1 thread: baseline performance (no parallelization)
- 2-4 threads: significant speedup on multi-core systems
- 8+ threads: diminishing returns due to overhead and core count limits

**Cache effects (most important finding):**
- **Strategy 1 (row-by-row)**: BEST - excellent cache locality since Java uses row-major order
- **Strategy 2 (column-by-column)**: SLOWER - more cache misses from column-wise access
- **Strategy 3 (k-th element)**: WORST - scattered memory access pattern destroys cache efficiency

**Matrix size:**
- Small matrices (< 100x100): Thread creation overhead dominates, sequential might be faster
- Large matrices (> 1000x1000): Parallelization benefits become clear

**Thread creation overhead:**
- Creating and synchronizing threads takes time
- More threads doesn't always mean faster execution

---

## Files in This Lab

```
ppd/lab03/
├── MatrixMultiply.java  # Main implementation
├── Benchmark.java       # Performance testing suite
├── Makefile            # Build automation (optional)
├── run.sh              # Interactive script
└── README.md           # This file
```

---

## Quick Reference

### Make Commands
| Command | Description |
|---------|-------------|
| `make` | Compile all Java files |
| `make run` | Run basic test (1000x1000, 4 threads) |
| `make debug` | Run with debug output (9x9, 4 threads) |
| `make benchmark` | Run full benchmark suite |
| `make clean` | Remove compiled .class files |

### Direct Java Commands
| Command | Description |
|---------|-------------|
| `javac *.java` | Compile all Java files |
| `java MatrixMultiply N M P T` | Run with NxM * MxP matrices, T threads |
| `java MatrixMultiply N M P T debug` | Run with debug output |
| `java Benchmark` | Run benchmark suite |
| `rm *.class` | Clean compiled files |

---

## Troubleshooting

**"javac: command not found"**
- Java JDK not installed or not in PATH
- If using jenv: `jenv enable-plugin export`

**"java.lang.OutOfMemoryError"**
- Matrix too large for available heap
- Reduce matrix size or increase heap: `java -Xmx4g MatrixMultiply ...`

**Slow performance**
- Normal for large matrices
- Try smaller sizes first to verify correctness

**jenv not working**
- Run `jenv versions` to see available versions
- Run `jenv local <version>` to set version for current directory
- Run `jenv enable-plugin export` to enable PATH export