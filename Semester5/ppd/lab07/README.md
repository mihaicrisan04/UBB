# Lab07 - MPI Polynomial Multiplication

Distributed polynomial multiplication using MPI with both naive O(n²) and Karatsuba O(n^1.585) algorithms.

## Prerequisites

- **C++17** compiler (g++, clang++)
- **CMake** 3.14+
- **OpenMPI** or equivalent MPI implementation

### macOS Installation

```bash
brew install cmake open-mpi
```

### Linux Installation

```bash
# Ubuntu/Debian
sudo apt-get install cmake libopenmpi-dev openmpi-bin

# Fedora
sudo dnf install cmake openmpi openmpi-devel
```

## Build

```bash
cd lab07
mkdir build && cd build
cmake ..
make
```

The executable will be in `build/bin/lab07`.

## Run

### Compare All Variants

```bash
mpirun -np 4 ./bin/lab07 --compare --size 5000 --repeats 5
```

### Run Specific Variant

```bash
# Naive sequential
mpirun -np 1 ./bin/lab07 --algorithm naive --mode seq --size 5000

# Naive MPI
mpirun -np 4 ./bin/lab07 --algorithm naive --mode mpi --size 5000

# Karatsuba sequential
mpirun -np 1 ./bin/lab07 --algorithm karatsuba --mode seq --size 10000

# Karatsuba MPI
mpirun -np 4 ./bin/lab07 --algorithm karatsuba --mode mpi --size 10000
```

### CLI Options

| Option | Description | Default |
|--------|-------------|---------|
| `--algorithm, -a` | Algorithm: `naive` or `karatsuba` | `naive` |
| `--mode, -m` | Mode: `seq` or `mpi` | `seq` |
| `--size, -n` | Polynomial size | 1000 |
| `--repeats, -r` | Benchmark repeats | 3 |
| `--compare` | Run all 4 variants | false |
| `--no-verify` | Skip verification | false |
| `--karatsuba-seq-base` | Karatsuba base case threshold | 64 |
| `--help, -h` | Show help | - |

## Example Output

```
Config:
  algorithm = naive
  mode      = seq
  procs     = 4
  size      = 5000
  repeats   = 3
  compare   = true
  verify    = true

naive seq                | avg: 45.123     ms  min: 44.567     ms
naive mpi                | avg: 12.456     ms  min: 11.890     ms
karatsuba seq            | avg: 8.234      ms  min: 7.890      ms
karatsuba mpi            | avg: 4.567      ms  min: 4.123      ms

Verification:
  naive mpi == naive seq:     OK
  karatsuba mpi == kara seq:  OK
  karatsuba seq == naive seq: OK
```

## Performance Benchmarking

Run benchmarks across different sizes and process counts:

```bash
for SIZE in 1000 5000 10000 50000; do
  for NP in 1 2 4 8; do
    echo "=== Size: $SIZE, Procs: $NP ==="
    mpirun -np $NP ./bin/lab07 --compare --size $SIZE --repeats 5
  done
done
```

## Comparison with Lab05

To compare with the Java implementation from lab05:

```bash
# Lab05 (Java)
cd ../lab05
mvn exec:java -Dexec.args="--compare --size 5000 --threads 4 --repeats 5"

# Lab07 (MPI)
cd ../lab07/build
mpirun -np 4 ./bin/lab07 --compare --size 5000 --repeats 5
```

## Documentation

See [DOCS.md](DOCS.md) for detailed algorithm descriptions, distribution strategies, and performance analysis.

## Project Structure

```
lab07/
├── CMakeLists.txt          # Build configuration
├── README.md               # This file
├── DOCS.md                 # Algorithm documentation
└── src/
    ├── main.cpp            # CLI and benchmark harness
    ├── polynomial.hpp      # Polynomial representation
    ├── polynomial.cpp
    ├── multiply.hpp        # Sequential algorithms
    ├── multiply.cpp
    ├── mpi_multiply.hpp    # MPI-distributed algorithms
    └── mpi_multiply.cpp
```
