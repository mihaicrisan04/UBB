# School Timetable Generator using Genetic Algorithms

Three parallel implementations of a timetable generator using genetic algorithms.

## Quick Start

```bash
mkdir build && cd build
cmake ..
make -j$(nproc)

# Run threaded version
./bin/timetable_threaded --benchmark

# Run MPI version
mpirun -np 4 ./bin/timetable_mpi

# Run OpenCL version
./bin/timetable_opencl
```

## Implementations

| Version | Parallelization | Best For |
|---------|----------------|----------|
| Threaded | std::thread/future | Multi-core CPU |
| MPI | Island model | Distributed systems |
| OpenCL | GPU compute | Large populations |

## Requirements

- CMake 3.14+
- C++17 compiler
- MPI (for MPI version)
- OpenCL (for OpenCL version, included in macOS)

See [DOCS.md](DOCS.md) for detailed documentation.
