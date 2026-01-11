# Distributed Shared Memory: In-Depth Documentation

## Problem Statement

Implement a distributed shared memory system where:
1. Multiple processes share integer variables
2. Any subscriber can write to a variable
3. All subscribers must see changes in the **same order** (total ordering)
4. Compare-and-exchange must be atomic
5. No central server bottleneck - only subscribers communicate

## The Core Challenge: Total Ordering

The fundamental difficulty in distributed systems is ensuring all nodes agree on the order of events. Consider:

```
Time →
Process A:  write(x, 1) ─────────────────────────────►
Process B:  ─────────────── write(x, 2) ─────────────►
```

Without coordination, Process A might see `x: 0→1→2` while Process B sees `x: 0→2→1`. This violates total ordering.

### Why Total Ordering Matters

If processes see different orderings:
- Replicated state diverges
- Application logic becomes unpredictable
- Debugging becomes impossible

## Solution: Coordinator-Based Sequencing

Each variable has a **coordinator** (the lowest-rank subscriber) that acts as a sequencer.

```
Variable 0: subscribers = {0, 1, 2} → coordinator = 0
Variable 1: subscribers = {1, 2, 3} → coordinator = 1
Variable 2: subscribers = {0, 3}    → coordinator = 0
```

The coordinator:
1. Receives all write requests for its variable
2. Assigns monotonically increasing sequence numbers
3. Broadcasts commits to all subscribers

This guarantees total ordering because all writes are serialized through one point.

## Message Protocol

### Message Structure

```cpp
struct Message {
    int type;        // Message type enum
    int var_id;      // Which variable
    int value;       // Value (write) or expected (CAS)
    int new_value;   // New value (CAS only)
    int seq_num;     // Sequence number (commits only)
    int sender;      // Original requester
    int request_id;  // Unique ID for tracking completion
};
```

### Message Types

| Type | Direction | Purpose |
|------|-----------|---------|
| `WRITE_REQUEST` | Requester → Coordinator | Request to write a value |
| `WRITE_COMMIT` | Coordinator → All Subscribers | Committed write with sequence number |
| `CAS_REQUEST` | Requester → Coordinator | Request compare-and-exchange |
| `CAS_COMMIT` | Coordinator → All Subscribers | Successful CAS with sequence number |
| `CAS_RESPONSE` | Coordinator → Requester | CAS failed (condition not met) |

## Write Operation Flow

### Case 1: Writer is Coordinator

```
┌─────────────────────────────────────────────────────────────┐
│ Process 0 (Coordinator for var 0)                           │
│                                                             │
│   write(0, 42)                                              │
│       │                                                     │
│       ▼                                                     │
│   seq = next_seq++  ─────► seq = 0                          │
│       │                                                     │
│       ▼                                                     │
│   broadcast WRITE_COMMIT(var=0, value=42, seq=0)            │
│       │                                                     │
│       ├────────────────────► Process 1                      │
│       │                                                     │
│       └────────────────────► Process 2                      │
│       │                                                     │
│       ▼                                                     │
│   apply_commit locally                                      │
│       │                                                     │
│       ▼                                                     │
│   callback(0, old_value, 42)                                │
└─────────────────────────────────────────────────────────────┘
```

### Case 2: Writer is Not Coordinator

```
┌──────────────────┐         ┌──────────────────┐         ┌──────────────────┐
│    Process 1     │         │ Process 0 (Coord)│         │    Process 2     │
│                  │         │                  │         │                  │
│  write(0, 42)    │         │                  │         │                  │
│       │          │         │                  │         │                  │
│       ▼          │         │                  │         │                  │
│  send REQUEST ───┼────────►│  receive REQUEST │         │                  │
│       │          │         │       │          │         │                  │
│       │          │         │       ▼          │         │                  │
│       │          │         │  seq = next_seq++│         │                  │
│       │          │         │       │          │         │                  │
│       │          │         │       ▼          │         │                  │
│       │          │◄────────┼── broadcast ─────┼────────►│  receive COMMIT  │
│       │          │         │    COMMIT        │         │       │          │
│       ▼          │         │       │          │         │       ▼          │
│  wait for COMMIT │         │       ▼          │         │  apply_commit    │
│       │          │         │  apply_commit    │         │       │          │
│       ▼          │         │                  │         │       ▼          │
│  receive COMMIT  │         │                  │         │  callback()      │
│       │          │         │                  │         │                  │
│       ▼          │         │                  │         │                  │
│  apply_commit    │         │                  │         │                  │
│       │          │         │                  │         │                  │
│       ▼          │         │                  │         │                  │
│  callback()      │         │                  │         │                  │
│       │          │         │                  │         │                  │
│       ▼          │         │                  │         │                  │
│  write() returns │         │                  │         │                  │
└──────────────────┘         └──────────────────┘         └──────────────────┘
```

## Sequence Number Ordering

Subscribers may receive commits out of order due to network timing. The DSM buffers and reorders:

```cpp
// In handle_message for WRITE_COMMIT:
if (msg.seq_num == var.last_applied_seq + 1) {
    // Next expected sequence - apply immediately
    apply_commit(var_id, msg);
    try_apply_pending(var_id);  // Check if buffered commits can now apply
} else if (msg.seq_num > var.last_applied_seq + 1) {
    // Future sequence - buffer it
    var.pending_commits[msg.seq_num] = msg;
}
// If seq_num <= last_applied_seq, it's a duplicate - ignore
```

### Example: Out-of-Order Delivery

```
Coordinator sends: seq=0, seq=1, seq=2

Process P receives: seq=0, seq=2, seq=1

Processing at P:
  1. Receive seq=0: last_applied=-1, expect 0 ✓ → apply, last_applied=0
  2. Receive seq=2: last_applied=0, expect 1 ✗ → buffer[2]=msg
  3. Receive seq=1: last_applied=0, expect 1 ✓ → apply, last_applied=1
     → try_apply_pending: buffer[2] exists, apply, last_applied=2

Final order seen by P: seq=0, seq=1, seq=2 ✓
```

## Compare-and-Exchange (CAS)

CAS is atomic because the coordinator performs the check-and-set as a single operation.

### Successful CAS

```
┌──────────────────┐         ┌──────────────────┐
│    Process 1     │         │ Process 0 (Coord)│
│                  │         │ var[0].value = 10│
│ CAS(0, 10, 99)   │         │                  │
│       │          │         │                  │
│       ▼          │         │                  │
│  send CAS_REQ ───┼────────►│  value == 10? YES│
│       │          │         │       │          │
│       │          │         │       ▼          │
│       │          │         │  value = 99      │
│       │          │         │  seq = next_seq++│
│       │          │         │       │          │
│       │          │◄────────┼── CAS_COMMIT ────┼────► other subscribers
│       ▼          │         │                  │
│  return true     │         │                  │
└──────────────────┘         └──────────────────┘
```

### Failed CAS

```
┌──────────────────┐         ┌──────────────────┐
│    Process 1     │         │ Process 0 (Coord)│
│                  │         │ var[0].value = 50│
│ CAS(0, 10, 99)   │         │                  │
│       │          │         │                  │
│       ▼          │         │                  │
│  send CAS_REQ ───┼────────►│  value == 10? NO │
│       │          │         │       │          │
│       │          │◄────────┼── CAS_RESPONSE ──┤ (no broadcast)
│       ▼          │         │   (failure)      │
│  return false    │         │                  │
└──────────────────┘         └──────────────────┘
```

### CAS Atomicity Guarantee

Because the coordinator:
1. Processes requests sequentially
2. Checks condition and sets value in one operation
3. Only then broadcasts the commit

No race condition can occur. Two concurrent CAS operations are serialized:

```
CAS(x, 0, 1) from P1  ──┐
                        ├──► Coordinator processes sequentially
CAS(x, 0, 2) from P2  ──┘

Result: One succeeds, one fails (depending on arrival order)
```

## The Sync Barrier

Standard `MPI_Barrier` blocks without processing DSM messages, causing deadlock:

```
P0 (coordinator): write() completes → MPI_Barrier (blocks)
P1: write() → sends REQUEST to P0 → waits for COMMIT (blocks)

Deadlock: P0 waits for P1 at barrier, P1 waits for P0 to process message
```

Solution: `sync()` uses non-blocking barrier with message processing:

```cpp
void DSM::sync() {
    MPI_Request barrier_req;
    MPI_Ibarrier(comm_, &barrier_req);  // Non-blocking barrier

    int complete = 0;
    while (!complete) {
        process_messages();  // Handle DSM messages
        MPI_Test(&barrier_req, &complete, MPI_STATUS_IGNORE);
    }
}
```

This allows the coordinator to process incoming requests while waiting for the barrier.

## Request Tracking

Each write/CAS operation gets a unique `request_id` to track completion:

```cpp
void DSM::write(int var_id, int value) {
    int req_id = next_request_id_++;  // Unique ID

    // ... send request with req_id ...

    // Wait for OUR specific request to complete
    while (completed_requests_.find(req_id) == completed_requests_.end()) {
        process_messages();
    }
}
```

When a COMMIT arrives with matching sender and request_id, we know our write completed.

## Why No Central Server?

The requirement states: "messages should be exchanged only between the subscribers of that variable."

With per-variable coordinators:
- Variable A (subscribers: {0,1,2}) → coordinator 0
- Variable B (subscribers: {3,4,5}) → coordinator 3

Operations on A never involve processes 3,4,5. Operations on B never involve 0,1,2. No single process handles all variables.

## Complexity Analysis

### Message Complexity per Write

| Operation | Messages |
|-----------|----------|
| Writer is coordinator | N-1 (broadcast to other subscribers) |
| Writer is not coordinator | 1 (request) + N-1 (broadcast) = N |

Where N = number of subscribers for that variable.

### Latency

| Operation | Round Trips |
|-----------|-------------|
| Writer is coordinator | 0 (local) |
| Writer is not coordinator | 1 (request → commit) |

### Space Complexity

Per variable:
- O(1) for value, coordinator, sequence counters
- O(M) for pending commits buffer (M = max out-of-order messages)

## Failure Handling

This implementation assumes no failures (as per requirements). In a production system, you would need:

1. **Coordinator failure**: Elect new coordinator from remaining subscribers
2. **Message loss**: Acknowledgments and retransmission
3. **Network partitions**: Consensus protocols (Paxos, Raft)

## Comparison with Alternatives

| Approach | Total Ordering | Latency | Complexity |
|----------|---------------|---------|------------|
| Central sequencer | Yes | 2 hops always | Low |
| Per-variable coordinator (this) | Yes | 1-2 hops | Medium |
| Lamport timestamps | Partial | 1 hop | High |
| Vector clocks | Partial | 1 hop | High |
| Consensus (Paxos/Raft) | Yes | Multiple rounds | Very High |

Our approach balances simplicity with the requirement of no central bottleneck.

## Code Structure

```
src/
├── dsm.hpp          # Public API and data structures
│   ├── MessageType  # Enum for message types
│   ├── Message      # Wire format for MPI messages
│   ├── Variable     # Per-variable state
│   └── DSM          # Main class
│
├── dsm.cpp          # Implementation
│   ├── subscribe()          # Configure variable subscribers
│   ├── write()              # Write operation
│   ├── compare_exchange()   # CAS operation
│   ├── sync()               # DSM-aware barrier
│   ├── process_messages()   # Poll and handle incoming
│   ├── handle_message()     # Dispatch by message type
│   ├── apply_commit()       # Apply change, invoke callback
│   └── try_apply_pending()  # Process buffered commits
│
└── main.cpp         # Demo and test harness
    ├── run_demo()           # Interactive demonstration
    └── run_ordering_test()  # Verify total ordering property
```

## Testing Total Ordering

The test has all processes perform writes, then compare their observed change sequences:

```cpp
// Each process records changes
mem.set_callback([](int var_id, int old_val, int new_val) {
    change_log.push_back({var_id, old_val, new_val});
});

// Multiple rounds of writes from different processes
for (int round = 0; round < 3; round++) {
    for (int v = 0; v < num_vars; v++) {
        if ((v + round) % size == rank) {
            mem.write(v, rank * 100 + round * 10 + v);
        }
    }
    mem.sync();
}

// Compare logs across all processes
// SUCCESS if all logs are identical
```

If total ordering is violated, different processes would have different log sequences.
