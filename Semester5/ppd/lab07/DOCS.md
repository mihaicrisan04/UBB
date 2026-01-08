# Lab07 Documentation - MPI Polynomial Multiplication

## Overview

This lab implements distributed polynomial multiplication using MPI. Two algorithms are provided:
- **Naive O(n²)** - Simple nested loop multiplication
- **Karatsuba O(n^1.585)** - Divide-and-conquer with reduced multiplications

Both algorithms are implemented in sequential and MPI-distributed variants.

---

## 1. Algorithms

### 1.1 Naive O(n²) Algorithm

**Definition:**

Given polynomials A(x) = Σ aᵢxⁱ and B(x) = Σ bⱼxʲ, the product C(x) = A(x)·B(x) has coefficients:

```
C[k] = Σ A[i] × B[k-i]  for 0 ≤ i < n and 0 ≤ k-i < m
```

**Pseudocode:**

```
function naive_multiply(A, B):
    n = size(A)
    m = size(B)
    C = new array[n + m - 1] initialized to 0
    
    for i = 0 to n-1:
        for j = 0 to m-1:
            C[i + j] += A[i] × B[j]
    
    return C
```

**Complexity:** O(n × m), typically O(n²) when |A| ≈ |B| ≈ n

### 1.2 Karatsuba Algorithm

**Key Insight:** Reduce 4 multiplications to 3 using algebraic identity.

**Algorithm:**

1. Split polynomials at midpoint m:
   - A = A₀ + A₁·xᵐ
   - B = B₀ + B₁·xᵐ

2. Compute three products:
   - z₀ = A₀ × B₀
   - z₂ = A₁ × B₁
   - z₁ = (A₀ + A₁) × (B₀ + B₁) - z₀ - z₂

3. Combine:
   - C = z₀ + z₁·xᵐ + z₂·x²ᵐ

**Pseudocode:**

```
function karatsuba(A, B, seqBase):
    n = size(A)
    
    if n ≤ seqBase:
        return naive_multiply(A, B)
    
    mid = n / 2
    
    A0 = A[0..mid-1]
    A1 = A[mid..n-1]
    B0 = B[0..mid-1]
    B1 = B[mid..n-1]
    
    z0 = karatsuba(A0, B0, seqBase)
    z2 = karatsuba(A1, B1, seqBase)
    z1 = karatsuba(A0+A1, B0+B1, seqBase) - z0 - z2
    
    return z0 + shift(z1, mid) + shift(z2, 2*mid)
```

**Complexity:** T(n) = 3T(n/2) + O(n) → O(n^log₂(3)) ≈ O(n^1.585)

---

## 2. Distribution and Communication

### 2.1 MPI Naive Distribution

**Strategy:** Partition output coefficient indices across processes.

```
┌─────────────────────────────────────────────────────┐
│                    Master (rank 0)                   │
│  1. Broadcast A and B to all workers                │
│  2. Partition output indices [0, n+m-2]             │
│  3. Each process computes C[k] for assigned range   │
│  4. Gather partial results                          │
└─────────────────────────────────────────────────────┘
        │
        │ MPI_Bcast (A, B)
        ▼
┌───────┬───────┬───────┬───────┐
│ Proc 0│ Proc 1│ Proc 2│ Proc 3│
│C[0..k]│C[k+1] │C[...] │C[..n] │
└───────┴───────┴───────┴───────┘
        │
        │ MPI_Gather (results)
        ▼
┌─────────────────────────────────────────────────────┐
│                    Master (rank 0)                   │
│  Assembles final result C                           │
└─────────────────────────────────────────────────────┘
```

**Communication Pattern:**

| Operation | MPI Function | Data |
|-----------|-------------|------|
| Input distribution | `MPI_Bcast` | Polynomials A, B |
| Result collection | `MPI_Gather` | Partial result arrays |

**Synchronization:**
- `MPI_Bcast` provides implicit barrier for input
- `MPI_Gather` provides implicit barrier for output
- No explicit locks needed (disjoint write regions)

### 2.2 MPI Karatsuba Distribution

**Strategy:** Distribute the three subproblems (z₀, z₁, z₂) across processes.

```
┌─────────────────────────────────────────────────────┐
│                    Master (rank 0)                   │
│  1. Broadcast padded A and B                        │
│  2. Split into halves: A0, A1, B0, B1              │
│  3. Assign subproblems based on rank % 3           │
└─────────────────────────────────────────────────────┘
        │
        │ MPI_Bcast (A_padded, B_padded)
        ▼
┌───────────┬───────────┬───────────┐
│  Proc 0   │  Proc 1   │  Proc 2   │
│ z0=A0×B0  │ z2=A1×B1  │z1=(A0+A1) │
│           │           │  ×(B0+B1) │
└───────────┴───────────┴───────────┘
        │
        │ MPI_Bcast (z0, z1, z2 from respective roots)
        ▼
┌─────────────────────────────────────────────────────┐
│                    All Processes                     │
│  Combine: C = z0 + z1·x^m + z2·x^(2m)              │
└─────────────────────────────────────────────────────┘
```

**Process Assignment:**
- `rank % 3 == 0` → computes z₀ = A₀ × B₀
- `rank % 3 == 1` → computes z₂ = A₁ × B₁
- `rank % 3 == 2` → computes z₁_full = (A₀+A₁) × (B₀+B₁)

**Fallback:** With fewer than 3 processes, all subproblems computed locally.

---

## 3. Performance Measurements

### 3.1 Methodology

- **Input:** Polynomials with all coefficients = 1
- **Timing:** `std::chrono::high_resolution_clock` (nanosecond precision)
- **Metrics:** Average and minimum time across R repeats
- **Verification:** Results compared against sequential naive implementation

### 3.2 Expected Results

| Size | Naive Seq | Naive MPI-4 | Speedup | Karatsuba Seq | Karatsuba MPI-4 | Speedup |
|------|-----------|-------------|---------|---------------|-----------------|---------|
| 1000 | ~X ms | ~Y ms | ~Zx | ~A ms | ~B ms | ~Cx |
| 5000 | ~X ms | ~Y ms | ~Zx | ~A ms | ~B ms | ~Cx |
| 10000 | ~X ms | ~Y ms | ~Zx | ~A ms | ~B ms | ~Cx |
| 50000 | ~X ms | ~Y ms | ~Zx | ~A ms | ~B ms | ~Cx |

*Fill in after running benchmarks on your hardware.*

### 3.3 Comparison with Lab05

Lab05 uses Java with thread-based parallelism (ExecutorService). Key differences:

| Aspect | Lab05 (Java Threads) | Lab07 (MPI) |
|--------|---------------------|-------------|
| Parallelism | Shared memory | Distributed memory |
| Communication | Implicit (shared arrays) | Explicit (message passing) |
| Synchronization | Futures, implicit barrier | MPI_Bcast, MPI_Gather |
| Overhead | Thread creation | Process creation + network |
| Scalability | Single machine | Multiple nodes |

**Expected observations:**
- For small inputs on single machine, Java threads may be faster (lower overhead)
- For large inputs or multi-node, MPI scales better
- Karatsuba advantage more pronounced for large sizes in both

### 3.4 Benchmark Commands

```bash
# Run full comparison
mpirun -np 4 ./bin/lab07 --compare --size 10000 --repeats 5

# Scaling test
for NP in 1 2 4 8; do
  echo "=== Processes: $NP ==="
  mpirun -np $NP ./bin/lab07 --algorithm naive --mode mpi --size 20000 --repeats 5
done

# Size scaling
for SIZE in 1000 5000 10000 50000; do
  echo "=== Size: $SIZE ==="
  mpirun -np 4 ./bin/lab07 --compare --size $SIZE --repeats 3
done
```

---

## 4. Bonus: Big Number Multiplication

Big integers can be represented as polynomials with base-B digits (limbs):

```
N = d₀ + d₁·B + d₂·B² + ... + dₖ·Bᵏ
```

where B is typically 10⁹ or 2³² for efficiency.

**Multiplication process:**
1. Represent numbers as polynomials of limbs
2. Multiply polynomials (using naive or Karatsuba)
3. Propagate carries through the result

**Carry propagation:**
```
for i = 0 to result.size - 1:
    carry = result[i] / BASE
    result[i] = result[i] % BASE
    result[i+1] += carry
```

This approach reuses the polynomial multiplication infrastructure for arbitrary-precision arithmetic.

---

## 5. Implementation Notes

### 5.1 Polynomial Representation

- Coefficients stored as `std::vector<long>` in increasing degree order
- `A(x) = 3 + 2x + 5x³` → `[3, 2, 0, 5]`
- Trailing zeros automatically trimmed

### 5.2 MPI Data Types

- Coefficient arrays use `MPI_LONG` (64-bit signed integer)
- Size metadata exchanged as `MPI_LONG` before data transfer

### 5.3 Error Handling

- MPI errors propagate via default error handler
- Result verification catches algorithmic bugs
- Invalid CLI arguments print usage and exit

---

## 6. References

1. Karatsuba, A. (1962). "Multiplication of Multidigit Numbers on Automata"
2. MPI Forum. "MPI: A Message-Passing Interface Standard"
3. Lab05 documentation for thread-based comparison baseline
