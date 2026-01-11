# Lab 08: Distributed Shared Memory (DSM)

A simple DSM mechanism with total ordering guarantees.

## Building

```bash
mkdir build && cd build
cmake ..
make
```

## Running

```bash
# Demo mode
mpirun -np 4 ./bin/lab08 --demo

# Total ordering test
mpirun -np 4 ./bin/lab08 --test

# With custom number of variables
mpirun -np 6 ./bin/lab08 --test --vars 5
```

## Architecture

### Coordinator-based Total Ordering

Each variable has a designated coordinator (lowest-rank subscriber). All writes go through the coordinator who assigns monotonically increasing sequence numbers, ensuring all subscribers see changes in the same order.

### Message Flow

**Write operation:**
1. Sender sends `WRITE_REQUEST` to coordinator
2. Coordinator assigns sequence number
3. Coordinator broadcasts `WRITE_COMMIT` to all subscribers
4. Subscribers apply changes in sequence order

**Compare-and-Exchange:**
1. Sender sends `CAS_REQUEST` to coordinator
2. Coordinator checks condition atomically
3. If condition met: broadcast `CAS_COMMIT`
4. If not met: send `CAS_RESPONSE` (failure)

### API

```cpp
dsm::DSM mem(MPI_COMM_WORLD, num_variables);

// Subscribe processes to a variable
mem.subscribe(var_id, {0, 1, 2});

// Set change notification callback
mem.set_callback([](int var_id, int old_val, int new_val) {
    // Handle change
});

// Operations
int val = mem.read(var_id);
mem.write(var_id, 42);
bool success = mem.compare_exchange(var_id, expected, new_value);

// DSM-aware synchronization barrier
mem.sync();
```

## Guarantees

- **Total ordering**: All subscribers see variable changes in the same order
- **Atomicity**: CAS operations are atomic at the coordinator
- **No central bottleneck**: Each variable has its own coordinator among subscribers
