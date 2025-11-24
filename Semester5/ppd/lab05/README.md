# Polynomial Multiplication (lab05)

A Java project that implements and compares 4 variants of polynomial multiplication:
- O(n²) naive: sequential and parallel
- Karatsuba O(n^1.585): sequential and parallel

Includes verification, CLI for benchmarking, and detailed performance measurement.

-------------------------------------------------------------------------------

## Project layout

```
lab05/
├── src/main/java/ppd/lab05/     Java sources
│   ├── Main.java                 CLI and benchmark harness
│   ├── Polynomial.java           Polynomial representation
│   └── PolynomialMultiply.java   Multiplication algorithms
├── target/                       Maven build directory
├── pom.xml                       Maven project configuration
├── .java-version                 jenv local Java version
└── README.md                     This document
```

-------------------------------------------------------------------------------

## Prerequisites

- **Java 25** (managed via jenv)
- **Maven 3.x**
- The project is configured to use Java 25 via `.java-version`

Verify your setup:
```bash
java -version   # Should show Java 25
mvn --version   # Should show Maven 3.x
```

-------------------------------------------------------------------------------

## Build

Maven handles compilation automatically.

**Compile sources:**
```bash
mvn compile
```

**Package JAR:**
```bash
mvn package
```

**Clean build:**
```bash
mvn clean
mvn clean package    # Clean and build
```

-------------------------------------------------------------------------------

## Run

The program provides a CLI for selecting algorithm, mode, input size, threads, etc.

### Quick examples:

**Compare all 4 variants:**
```bash
mvn exec:java -Dexec.args="--compare --size 4000 --threads 8 --repeats 3"

# Or use the predefined profile:
mvn exec:java@compare
```

**Run specific variants:**
```bash
# Naive sequential
mvn exec:java -Dexec.args="--algorithm naive --mode seq --size 4000"

# Naive parallel
mvn exec:java -Dexec.args="--algorithm naive --mode par --threads 8 --size 10000"

# Karatsuba sequential
mvn exec:java -Dexec.args="--algorithm karatsuba --mode seq --size 20000"

# Karatsuba parallel
mvn exec:java -Dexec.args="--algorithm karatsuba --mode par --threads 8 --size 50000"
```

**Using predefined executions:**
```bash
mvn exec:java@naive-seq
mvn exec:java@naive-par
mvn exec:java@karatsuba-seq
mvn exec:java@karatsuba-par
```

**Run from JAR:**
```bash
mvn package
java -jar target/lab05.jar --compare --size 4000 --threads 8
```

**Show help:**
```bash
mvn exec:java -Dexec.args="--help"
```

### CLI flags:

- `--algorithm <naive|karatsuba>` - Algorithm to use
- `--mode <seq|par>` - Sequential or parallel mode
- `--threads N` - Number of threads (default: CPU count)
- `--size N` - Polynomial size (default: 4000)
- `--repeats R` - Benchmark repeats (default: 3)
- `--seed SEED` - Random seed (default: current time)
- `--coeff-range V` - Coefficient range [-V, V] (default: 1000)
- `--compare` - Run all 4 variants on same input
- `--no-verify` - Skip result verification
- `--karatsuba-seq-base B` - Sequential base case threshold (default: 64)
- `--karatsuba-par-base B` - Parallel threshold (default: 256)
- `--karatsuba-max-depth D` - Max parallel recursion depth (default: ceil(log₂(threads)))
- `--help` - Show help message

-------------------------------------------------------------------------------

## Polynomial representation

Polynomials are represented as arrays of `long` coefficients in increasing degree order:
- `A(x) = 3 + 2x + 5x³` → `A = [3, 2, 0, 5]`
- Trailing zeros are automatically trimmed to maintain correct degree

**Overflow considerations:**
- Default coefficient range: [-1000, 1000]
- With moderate sizes, 64-bit `long` is sufficient
- For very large sizes or coefficient ranges, consider switching to `BigInteger`

-------------------------------------------------------------------------------

## Algorithms

### 1) Naive O(n²) Algorithm

**Definition:**
```
C[k] = Σ A[i] × B[k - i]  for all valid i
```
where `0 ≤ i < |A|` and `0 ≤ k - i < |B|`

**Complexity:** O(n × m), typically O(n²) when |A| ≈ |B| ≈ n

**Sequential implementation:**
- Nested loops computing each output coefficient
- Straightforward and efficient for small to medium inputs

**Parallel implementation:**
- **Partitioning strategy**: Divide output index space [0, n+m-2] across threads
- Each thread computes a disjoint range of result coefficients
- **Synchronization**: None required for result array (no concurrent writes)
- Implicit barrier: Parent thread waits for all futures to complete
- **Thread pool**: Uses `ExecutorService` with fixed thread pool
- **Scalability**: Good speedup until memory bandwidth becomes bottleneck

**When to use:**
- Small to medium polynomial sizes (< 10,000 coefficients)
- When simplicity and predictability are important
- Naive parallel often outperforms sequential naive significantly

### 2) Karatsuba Algorithm

**Idea:**
Divide-and-conquer approach that reduces three multiplications instead of four:

1. Split polynomials: `A = A₀ + A₁·x^m`, `B = B₀ + B₁·x^m`
2. Compute three products:
   - `z₀ = A₀ × B₀`
   - `z₂ = A₁ × B₁`
   - `z₁ = (A₀ + A₁) × (B₀ + B₁) - z₀ - z₂`
3. Recombine: `C = z₀ + z₁·x^m + z₂·x^(2m)`

**Complexity:** T(n) = 3T(n/2) + O(n) → **O(n^log₂(3)) ≈ O(n^1.585)**

**Sequential implementation:**
- Recursive divide-and-conquer
- Switches to naive multiplication below threshold (`seqBase`)
- Input is padded to power-of-2 size for clean splitting

**Parallel implementation:**
- **Parallelization points**: Independent subproblems z₀ and z₂
- z₀ and z₂ submitted as parallel tasks to thread pool
- z₁ computed in parent thread (or submitted depending on depth)
- **Synchronization**: Futures provide synchronization; parent waits for subresults
- **Recombination**: Happens in parent thread with exclusive access to result buffer
- **Control parameters**:
  - `maxDepth`: Limits parallel recursion depth (default: ceil(log₂(threads)))
  - `parBase`: Minimum size to enable parallelism (default: 256)
  - `seqBase`: Threshold for switching to naive (default: 64)

**When to use:**
- Large polynomial sizes (> 10,000 coefficients)
- Asymptotic advantage becomes significant
- Parallel version requires tuning for optimal performance

-------------------------------------------------------------------------------

## Parallelization and Synchronization

### Thread Pool (ExecutorService)

Both parallel variants use Java's `ExecutorService` with a fixed thread pool:
- **Creation**: `Executors.newFixedThreadPool(numThreads)`
- **Task submission**: Tasks return `Future<T>` for result retrieval
- **Synchronization**: `future.get()` blocks until task completes
- **Cleanup**: `executor.shutdown()` in finally block ensures proper cleanup

### Naive Parallel Synchronization

**Strategy:**
1. Result array `C` is allocated upfront with size `n + m - 1`
2. Output space partitioned: each thread assigned disjoint index range
3. Each thread computes `C[k]` for its assigned range of k values
4. No locks or atomic operations needed (disjoint writes)
5. Implicit barrier: main thread waits for all futures via `future.get()`

**Why it's safe:**
- Each `C[k]` is written by exactly one thread
- No data races or concurrent modifications
- Memory visibility guaranteed by `Future` happens-before relationship

**Performance characteristics:**
- Embarrassingly parallel for large inputs
- Limited by memory bandwidth and cache coherence
- Best speedup with 4-16 threads on modern CPUs

### Karatsuba Parallel Synchronization

**Strategy:**
1. At each recursion level (if depth < maxDepth and size ≥ parBase):
   - Submit z₀ = A₀ × B₀ as parallel task
   - Submit z₂ = A₁ × B₁ as parallel task
   - Compute z₁ in current thread (or submit as well)
2. Wait for z₀ and z₂ via `future.get()` (synchronization point)
3. Compute z₁ adjustment: z₁ = z₁_full - z₀ - z₂
4. Recombine in current thread (exclusive access, no sharing)

**Why it's safe:**
- Independent subproblems have no shared state
- Results communicated via futures (thread-safe)
- Recombination in single thread eliminates concurrent writes
- Depth limit prevents oversubscription

**Performance characteristics:**
- Limited parallelism (3^depth tasks at depth d)
- Diminishing returns beyond log₂(cores) depth
- Overhead of task submission can hurt for small subproblems
- Best with tuned `parBase` and `maxDepth`

### Memory Model Guarantees

Java's memory model provides:
- **Happens-before** relationship between task submission and completion
- **Visibility**: Changes made in submitted task visible to caller after `future.get()`
- **Atomicity**: Individual writes to distinct array indices are atomic for `long[]`
- **No need for volatile/synchronized**: Thread pool and futures handle synchronization

-------------------------------------------------------------------------------

## Performance Measurement

### Methodology

Each benchmark run:
1. Generates random polynomials A and B with specified size and coefficient range
2. Runs the selected algorithm variant R times (repeats)
3. Records wall-clock time for each run in nanoseconds
4. Computes average and minimum time across repeats
5. Verifies correctness by comparing against naive sequential (if `--verify` enabled)

### Metrics Reported

- **Average time**: Mean of all repeats (accounts for variance)
- **Minimum time**: Best case (useful to see potential with warm caches)
- Format: milliseconds with 3 decimal places

### How to Conduct Performance Analysis

**1. Choose test configurations:**
```bash
# Small input (naive should dominate)
mvn exec:java -Dexec.args="--compare --size 1000 --threads 8 --repeats 5"

# Medium input (transition point)
mvn exec:java -Dexec.args="--compare --size 10000 --threads 8 --repeats 5"

# Large input (Karatsuba advantage)
mvn exec:java -Dexec.args="--compare --size 50000 --threads 8 --repeats 5"

# Very large (Karatsuba parallel shines)
mvn exec:java -Dexec.args="--compare --size 200000 --threads 16 --repeats 3"
```

**2. Vary thread counts:**
```bash
for T in 1 2 4 8 16; do
  echo "=== Threads: $T ==="
  mvn exec:java -Dexec.args="--algorithm naive --mode par --size 20000 --threads $T --repeats 5"
done
```

**3. Tune Karatsuba parameters:**
```bash
mvn exec:java -Dexec.args="--algorithm karatsuba --mode par --size 100000 \
  --threads 8 --karatsuba-seq-base 32 \
  --karatsuba-par-base 512 --karatsuba-max-depth 3"
```

### Expected Observations

**Naive seq vs naive par:**
- Speedup scales well up to 8-16 threads
- Beyond that, memory bandwidth limits
- Very small inputs: parallel has overhead, sequential faster

**Naive seq vs Karatsuba seq:**
- Crossover point typically around n = 5,000 - 20,000
- Below crossover: naive faster (lower constants)
- Above crossover: Karatsuba faster (better asymptotic complexity)

**Karatsuba seq vs Karatsuba par:**
- Modest speedups (1.5x - 3x typically)
- Diminishing returns beyond maxDepth ≈ 3-4
- Requires large enough input (size > 50,000)

**Overall winner:**
- Small inputs (< 5k): Naive sequential or naive parallel
- Large inputs (> 50k): Karatsuba parallel (with tuning)

### Reporting Results

Include in your report:
- **Machine specs**: CPU model, core/thread count, RAM
- **OS and Java version**: `uname -a` and `java -version`
- **Commands used**: Exact Maven commands
- **Table format**:

| Size | Algorithm | Mode | Threads | Avg (ms) | Min (ms) | Speedup |
|------|-----------|------|---------|----------|----------|---------|
| 4000 | naive     | seq  | -       | 12.345   | 12.001   | 1.0x    |
| 4000 | naive     | par  | 8       | 2.456    | 2.301    | 5.0x    |
| ...  | ...       | ...  | ...     | ...      | ...      | ...     |

- **Analysis**: Discuss crossover points, scaling behavior, and tuning insights

-------------------------------------------------------------------------------

## Verification

Correctness is verified by comparing results:

**With `--compare` flag:**
- naive par == naive seq
- karatsuba par == karatsuba seq
- karatsuba seq == naive seq

**Single runs:**
- Result compared against naive sequential (reference implementation)

**If mismatch detected:**
- First 10 coefficients printed for debugging
- Common causes: overflow, incorrect partitioning, race condition

**Disable verification:**
```bash
mvn exec:java -Dexec.args="--compare --size 100000 --no-verify"
```

-------------------------------------------------------------------------------

## Implementation Details

### Polynomial class
- Immutable representation with `long[]` coefficients
- Automatic trimming of trailing zeros
- Static factory method for random generation
- Equals/hashCode for verification
- Utility methods: add, subtract, shiftLeft

### PolynomialMultiply class
- Pure static methods (no state)
- Four public APIs: `multiplyNaiveSeq`, `multiplyNaivePar`, `multiplyKaratsubaSeq`, `multiplyKaratsubaPar`
- Helper methods for array operations (add, subtract, naive small)
- Clear separation between sequential and parallel logic

### Main class
- Argument parsing with validation
- Benchmark harness with timing and statistics
- Result verification and reporting
- Configuration struct for clean parameter passing

### Thread Safety
- No shared mutable state between concurrent tasks
- Disjoint write guarantees in naive parallel
- Future-based synchronization in Karatsuba parallel
- Thread pool properly shutdown in finally blocks

-------------------------------------------------------------------------------

## Bonus: Big Numbers (BigInteger)

To handle arbitrarily large integers:

**Approach 1: Use BigInteger**
```java
// Change Polynomial to use BigInteger instead of long
private final BigInteger[] coeffs;

// Adapt all arithmetic operations
result[i + j] = result[i + j].add(ai.multiply(bj));
```

**Pros:**
- Correctness guaranteed for any size
- No overflow concerns
- Simple to implement

**Cons:**
- 10-100x slower than primitive `long`
- GC pressure from object allocation

**Approach 2: Limb-based representation**
- Represent big integers as arrays of base-B digits (e.g., base 10^9)
- Multiplication becomes polynomial convolution on limbs
- Requires carry propagation after multiplication
- Can reuse exact same Karatsuba/naive algorithms
- More complex but much faster than BigInteger for very large numbers

**Note:** This lab uses `long` for simplicity and performance. For bonus points, extend with BigInteger support.

-------------------------------------------------------------------------------

## Maven Cheat Sheet

**Common commands:**
```bash
mvn clean               # Clean build directory
mvn compile             # Compile sources
mvn package             # Build JAR
mvn clean package       # Clean and build
mvn exec:java           # Run main class
mvn dependency:tree     # Show dependencies (none in this project)
mvn help:effective-pom  # Show effective POM
```

**IDE Integration:**
```bash
# Import into IntelliJ IDEA:
# File → Open → Select pom.xml

# Import into Eclipse:
# File → Import → Existing Maven Projects

# Import into VS Code:
# Open folder, Java extension will detect pom.xml
```

-------------------------------------------------------------------------------

## Troubleshooting

**Issue: "jenv: version '25' not installed"**
- Solution: Install Java 25 and add to jenv: `jenv add /path/to/java25`

**Issue: Maven not found**
- macOS: `brew install maven`
- Linux: `sudo apt-get install maven` or `sudo yum install maven`
- Windows: Download from https://maven.apache.org/

**Issue: Poor parallel speedup**
- Increase input size (try size=20000 or larger)
- Adjust thread count to match CPU cores
- For Karatsuba parallel, tune `parBase` and `maxDepth`
- Check CPU throttling or background processes

**Issue: Verification mismatch**
- Reduce coefficient range to avoid overflow: `--coeff-range 100`
- Use smaller sizes for debugging: `--size 100`
- Check for race conditions (should not happen with current implementation)

**Issue: Out of memory**
- Reduce size
- Increase JVM heap: `export MAVEN_OPTS="-Xmx4g"`
- Or: `mvn exec:java -Dexec.args="..." -Dexec.jvmArgs="-Xmx4g"`

**Issue: IDE not recognizing source files**
- Make sure you opened the `lab05` folder (where `pom.xml` is)
- If using Zed, the language server should auto-detect Maven project
- Try reimporting the Maven project in your IDE

-------------------------------------------------------------------------------

## Further Optimizations

Possible extensions for advanced study:
1. **FFT-based multiplication**: O(n log n) using Fast Fourier Transform
2. **ForkJoinPool**: Use work-stealing pool instead of fixed thread pool
3. **Parallel streams**: Rewrite naive parallel using Java streams
4. **Cache-aware partitioning**: Optimize for L1/L2 cache sizes
5. **SIMD**: Use Vector API (Java 16+) for vectorized arithmetic
6. **GPU acceleration**: Offload to CUDA/OpenCL for massive parallelism
7. **Distributed**: Split work across multiple machines with message passing

-------------------------------------------------------------------------------

## License

Educational use for the PPD lab. Adapt as needed for coursework submissions.

-------------------------------------------------------------------------------

## Author

Implemented in Java 25 with Maven for PPD Lab 05 - Polynomial Multiplication
