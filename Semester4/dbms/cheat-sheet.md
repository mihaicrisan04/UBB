Okay, Mihai, here's a cheat sheet based on your lecture slides, designed to help you solve exercises similar to those in Lecture 13. I've focused on extracting the core concepts, definitions, and formulas needed for problem-solving, with references to the specific lectures and slides.

## DBMS Cheat Sheet (Based on Lectures 1-12)

---

### **Section 1: Transactions & Concurrency (Lectures 1, 2, 3)**

#### **1.1. Basic Concepts (Lecture 1)**

*   **Transaction:** A sequence of one or more operations on a database (read/write/commit/abort) that is an atomic unit of work. (L1, S11)
    *   **Operations:** `read(T, I)`, `write(T, I)`, `commit(T)`, `abort(T)`. (L1, S12)
*   **ACID Properties (L1, S13-14):**
    *   **Atomicity:** All operations of a transaction are executed, or none are (all-or-nothing). If a transaction fails, partial effects are undone. (L1, S15-16)
    *   **Consistency:** A transaction preserves database consistency. (L1, S17-19)
    *   **Isolation:** A transaction is protected from effects of concurrently scheduling other transactions; appears to run in single-user mode. (L1, S20)
    *   **Durability:** Effects of a successfully completed (committed) transaction persist even if system crashes. (L1, S21)
*   **Concurrency Anomalies (L1, S4-S6, S26-S29):**
    *   **Lost Update:** One transaction's update is overwritten by another. (Airline example L1, S4-5)
    *   **Incorrect Summary:** One transaction reads data while another is updating it, leading to an inconsistent view. (Bank example L1, S6)
    *   **Conflicts (Order of execution is important if on same data object, at least one write):** (L1, S26)
        *   **WR Conflict (Write-Read):** T2 reads an item previously written by T1.
            *   **Dirty Read:** T2 reads uncommitted data from T1. If T1 aborts, T2 has read invalid data. (L1, S27)
        *   **RW Conflict (Read-Write):** T2 writes an item previously read by T1.
            *   **Unrepeatable Read:** T1 reads an item, T2 modifies it, T1 re-reads and gets a different value. (L1, S28)
        *   **WW Conflict (Write-Write):** T2 writes an item previously written by T1.
            *   **Overwriting Uncommitted Data (Lost Update variant):** T1 writes, T2 overwrites T1's write before T1 commits. (L1, S29)

#### **1.2. Schedules & Serializability (Lecture 1, 2)**

*   **Schedule:** A list of operations from a set of transactions where the order of operations within each individual transaction is preserved. (L1, S30)
*   **Serial Schedule:** Actions of different transactions are not interleaved. (L1, S32)
*   **Non-Serial Schedule:** Actions of different transactions are interleaved. (L1, S33)
*   **Serializable Schedule:** A schedule $S$ whose effect on a consistent database is identical to the effect of some serial schedule $S_0$. (L1, S34)
    *   Ensures consistency. (L1, S35)

*   **Conflict Equivalence (S1 $\equiv_c$ S2) (L2, S3):**
    1.  S1 and S2 contain the same operations of the same transactions.
    2.  Every pair of conflicting operations is ordered in the same manner in S1 and S2.
*   **Conflict Serializable Schedule (L2, S6):** A schedule $S$ is conflict serializable if it is conflict equivalent to some serial schedule $S_0$.
*   **Precedence Graph (Serializability Graph) (L2, S8):**
    *   **Nodes:** One for each committed transaction in the schedule.
    *   **Arc (Ti $\rightarrow$ Tj):** If an action in Ti precedes and conflicts with an action in Tj.
    *   **Theorem:** A schedule $S$ is conflict serializable IF AND ONLY IF its precedence graph is acyclic. (L2, S8)
    *   **Algorithm to test conflict serializability (L2, S10):**
        1.  Create nodes for committed transactions.
        2.  Add arc Ti $\rightarrow$ Tj if Tj R(A) after Ti W(A).
        3.  Add arc Ti $\rightarrow$ Tj if Tj W(A) after Ti R(A).
        4.  Add arc Ti $\rightarrow$ Tj if Tj W(A) after Ti W(A).
        5.  If graph has no cycles, S is conflict serializable.
        *Example of non-conflict serializable schedule due to cycle: (L2, S9)*

*   **View Serializable Schedule (L2, S14, S18):** A schedule $S$ is view serializable if it is view equivalent to some serial schedule $S_0$.
    *   View equivalence (S1 $\equiv_v$ S2) (L2, S15):
        1.  If Ti reads initial value of V in S1, then Ti reads initial value of V in S2.
        2.  If Ti reads value of V written by Tj in S1, then Ti reads value of V written by Tj in S2.
        3.  If Ti writes final value of V in S1, then Ti writes final value of V in S2.
    *   Conflict serializability $\implies$ View serializability $\implies$ Serializability. (L2, S13, S19)

#### **1.3. Recoverability (Lecture 2)**

*   **Recoverable Schedule (L2, S22):** A transaction T commits only after all transactions whose changes T read also commit.
    *   *Unrecoverable example: T2 reads from T1, T2 commits, then T1 aborts. (L2, S20-21)*
*   **Avoiding Cascading Aborts (ACA) (L2, S23):** A schedule where transactions only read changes of committed transactions. ACA schedules are recoverable.
*   **Strict Schedules (L3, S12):** If Ti writes object A, then Tj can read/write A only after Ti completes (commits/aborts).
    *   Avoid cascading aborts.
    *   Are recoverable.
    *   Operations can be undone if aborted.

#### **1.4. Lock-Based Concurrency Control (Lecture 2, 3)**

*   **Lock Types (L2, S25):**
    *   **SLock (Shared/Read Lock):** Can read, cannot modify. Multiple transactions can hold SLocks simultaneously on the same object.
    *   **XLock (Exclusive/Write Lock):** Can read and write. Only one transaction can hold an XLock on an object.
*   **Lock Compatibility Matrix (L2, S25):**

    | Requested | Shared (Current) | Exclusive (Current) |
    | :-------- | :--------------- | :------------------ |
    | Shared    | Yes              | No                  |
    | Exclusive | No               | No                  |
*   **Two-Phase Locking (2PL) (L3, S8-S9):**
    1.  Before R/W, acquire S/X lock.
    2.  Once a lock is released, no other locks can be requested.
    *   **Phases:** Growing (acquires locks), Shrinking (releases locks).
    *   Guarantees serializability.
    *   Does NOT prevent cascading aborts (problem if locks released before commit). (L3, S10-11)
*   **Strict Two-Phase Locking (Strict 2PL) (L3, S3-S6):**
    1.  Before R/W, acquire S/X lock.
    2.  All locks held by a transaction are released ONLY when it completes (commits/aborts).
    *   Guarantees serializable and strict schedules (hence recoverable, ACA).
    *   *Example: T1 XLock(CM), T2 requests XLock(CM) $\implies$ T2 waits. T1 commits, releases locks. T2 proceeds. (L3, S5-6)*

#### **1.5. Deadlocks (Lecture 3)**

*   **Deadlock:** Cycle of transactions waiting for one another to release a locked resource. (L3, S13)
*   **Deadlock Prevention (L3, S15-S19):** Assign timestamp-based priorities (lower timestamp = older = higher priority).
    *   **Wait-Die:** If T1 (higher priority) requests lock held by T2 (lower priority), T1 waits. If T1 (lower priority) requests lock held by T2 (higher priority), T1 aborts ("dies"). (L3, S16-17)
    *   **Wound-Wait:** If T1 (higher priority) requests lock held by T2 (lower priority), T2 aborts ("is wounded"). If T1 (lower priority) requests lock held by T2 (higher priority), T1 waits. (L3, S18)
*   **Deadlock Detection (L3, S20-S24):**
    *   **Waits-For Graph:** Nodes are active transactions. Arc Ti $\rightarrow$ Tj if Ti waits for Tj to release a lock. Cycle $\implies$ deadlock. (L3, S20-S22)
    *   **Timeout Mechanism:** If T waits too long, assume deadlock and abort T. (L3, S25)

#### **1.6. Isolation Levels (SQL) (Lecture 3)**

*   Determines degree a transaction is exposed to others. (L3, S31)
*   `SET TRANSACTION ISOLATION LEVEL <isolevel>` (L3, S32)
*   **Dirty Writes are NOT allowed under ANY isolation level.** (L3, S32)

    | Isolation Level     | Dirty Reads | Unrepeatable Reads | Phantoms | S-Locks for Read | X-Locks for Write | Shared Locks Released | Exclusive Locks Released |
    | :------------------ | :---------- | :----------------- | :------- | :--------------- | :---------------- | :-------------------- | :----------------------- |
    | READ UNCOMMITTED    | Yes         | Yes                | Yes      | No               | Yes               | N/A (no S-locks)      | End of transaction       |
    | READ COMMITTED      | No          | Yes                | Yes      | Yes              | Yes               | Immediately           | End of transaction       |
    | REPEATABLE READ     | No          | No                 | Yes      | Yes              | Yes               | End of transaction    | End of transaction       |
    | SERIALIZABLE        | No          | No                 | No       | Yes (on objects & sets) | Yes          | End of transaction    | End of transaction       |
    *(Info synthesized from L3, S33-S37)*

*   **Phantom Problem (L3, S26-S30):** T1 reads a set of rows satisfying a predicate. T2 inserts/deletes a row satisfying that predicate. T1 re-executes query and gets a different set of rows.
    *   Conflict serializability does not guarantee serializability in presence of inserts/deletes if phantoms occur. (L3, S29)
    *   SERIALIZABLE isolation level prevents phantoms by locking sets of objects based on search predicates. (L3, S36)

---

### **Section 2: Crash Recovery (ARIES) (Lecture 4)**

*   **Goal:** Ensure Atomicity (undo uncommitted) & Durability (committed survive). (L4, S2)
*   **Buffer Manager Policies (L4, S7-S8):**
    *   **Steal:** Changes can be written to disk before commit. (Requires UNDO on recovery for aborted transactions whose changes hit disk).
    *   **No-Steal:** Changes cannot be written to disk before commit. (Simplifies UNDO).
    *   **Force:** Changes immediately forced to disk on commit. (Simplifies REDO for committed transactions).
    *   **No-Force:** Changes not forced to disk on commit. (Requires REDO for committed transactions whose changes didn't hit disk).
    *   **ARIES uses Steal/No-Force.** (L4, S10)
*   **Write-Ahead Logging (WAL) (L4, S10):**
    1.  Change to object O is first recorded in a log record (LR).
    2.  LR must be written to stable storage BEFORE the change to O is written to disk.
*   **Log Components (L4, S12-S17):**
    *   **LSN (Log Sequence Number):** Unique, monotonically increasing ID for each log record.
    *   **pageLSN:** Stored on each data page; LSN of the most recent log record describing a change to that page.
    *   **Log Record Fields:** `prevLSN` (links log records of same transaction), `transID`, `type`.
    *   **Update Log Record:** `pageID`, `length`, `offset`, `before-image`, `after-image`.
    *   **Compensation Log Record (CLR):** Written when an update is undone. Contains `undoNextLSN` (points to `prevLSN` of the undone update record). CLRs are redo-only.
*   **Key Data Structures for ARIES (L4, S18-S19):**
    *   **Transaction Table:** 1 entry/active transaction. Fields: `transID`, `status` (in progress, committed, aborted), `lastLSN` (LSN of most recent log record for this transaction).
    *   **Dirty Page Table (DPT):** 1 entry/dirty page in buffer pool. Fields: `pageID`, `recLSN` (LSN of the *first* log record that made the page dirty).
*   **Checkpointing (ARIES) (L4, S20-S21):** Reduces recovery work.
    1.  Write `begin_checkpoint` record.
    2.  Write `end_checkpoint` record (includes current Transaction Table & DPT).
    3.  Write `master` record (points to `begin_checkpoint` LSN) to a known place.
*   **ARIES Recovery Phases (L4, S10, S22-S33):**
    1.  **Analysis Phase (L4, S24-S28):**
        *   Find most recent `begin_checkpoint`. Get `end_checkpoint` (EC).
        *   Initialize Transaction Table and DPT from EC.
        *   Scan log forward from `begin_checkpoint` LSN:
            *   For `end` log record for T: Remove T from Transaction Table.
            *   For other log records (LR) for T: Add T to Transaction Table if not present. Set T.lastLSN = LR.LSN. If LR is `commit`, T.status = C. Else, T.status = U (undone).
            *   For redoable LR for page P: If P not in DPT, add P to DPT, set P.recLSN = LR.LSN.
        *   At end: Transaction Table has loser transactions (status U). DPT has potentially dirty pages.
    2.  **Redo Phase (Repeating History) (L4, S29-S32):**
        *   Start scan from smallest `recLSN` in DPT from Analysis.
        *   For each redoable log record LR (update or CLR) affecting page P:
            *   Redo action IF:
                1.  P is in DPT, AND
                2.  P.recLSN $\le$ LR.LSN, AND
                3.  P.pageLSN (on disk) < LR.LSN.
            *   To redo: Reapply logged action, set P.pageLSN = LR.LSN. No additional logging.
        *   At end of Redo: For every transaction T with status C, write `end` log record, remove T from Transaction Table.
    3.  **Undo Phase (L4, S33-S35):**
        *   ToUndo = { T.lastLSN | T is a loser transaction (status U in Transaction Table from Analysis) }.
        *   Repeat until ToUndo is empty:
            *   Choose largest LSN (L) in ToUndo. Let T be its transaction.
            *   If L is a CLR:
                *   If CLR.undoNextLSN is NULL, write `end` log record for T. Remove L from ToUndo.
                *   Else, add CLR.undoNextLSN to ToUndo. Remove L from ToUndo.
            *   If L is an update log record:
                *   Write a CLR for this update. CLR.undoNextLSN = L.prevLSN.
                *   Undo the update (using before-image). Set pageLSN = CLR.LSN.
                *   Remove L from ToUndo. Add L.prevLSN to ToUndo (if not NULL).
        *   *Crashes during Undo: ARIES handles this correctly by redoing CLRs and continuing Undo. (L4, S36-S43)*

---

### **Section 3: Query Processing & Optimization (Lectures 6, 7, 8, 9)**

#### **3.1. Access Paths & Selections (Lecture 6, 7)**

*   **Access Path:** Way of retrieving tuples (file scan or index scan). (L6, S5)
*   **Matching Selection Condition C to Index I (L6, S6-S10, S14-S15):**
    *   **Single attribute `attr op value`:**
        *   Tree index on `attr`: Matches for any `op`.
        *   Hash index on `attr`: Matches only if `op` is `=`.
    *   **Conjunctive condition $\bigwedge T_i$, where $T_i$ is `attr = value`:**
        *   Hash index on $\langle a, b, c \rangle$: Matches if C has `a=v1 AND b=v2 AND c=v3`. (L6, S9)
        *   B+ tree index on $\langle a, b, c \rangle$: Matches if C has terms for a prefix of $\langle a, b, c \rangle$ (e.g., `a=v1`, or `a=v1 AND b=v2`). (L6, S10)
    *   **General conditions in CNF:** (L6, S13)
        *   `(EDate < '4-1-2021' ∨ CID = 5) ∧ (Grade = 10 ∨ CID = 5)`
        *   Index can match parts of conjuncts. (L6, S14)
*   **Selectivity:** Number of pages retrieved (data + index). Most selective = fewest pages. (L6, S11)
*   **Selection Algorithms (L7, S29-S39):**
    *   **No index, unsorted:** File scan. Cost = M I/Os.
    *   **No index, sorted on `attr`:** Binary search (cost $\log_2 M$) + scan.
    *   **B+ tree index on `attr`:**
        *   Search tree (2-3 I/Os) + scan leaf pages + retrieve data pages.
        *   Clustered: Good for range, good for equality.
        *   Unclustered: Can be expensive for range if many tuples qualify.
    *   **General Selections (CNF without disjunctions):**
        1.  Use most selective access path (index or file scan) for one conjunct, apply rest.
        2.  Use multiple indexes (if a2/a3): Get rids, intersect rids, retrieve tuples, apply rest.
    *   **General Selections (CNF with disjunctions `J1 ∨ J2 ...`):**
        *   If any term in a disjunct requires file scan, that disjunct likely requires file scan.
        *   If all terms in a disjunct match indexes: retrieve tuples using indexes, compute union.

#### **3.2. Join Algorithms (Lecture 6, 7)**

*Assumptions: E (M pages, pE recs/page), S (N pages, pS recs/page). Join on Ei = Sj.*

*   **Simple Nested Loops Join (Tuple-oriented) (L6, S19):**
    *   For each tuple e $\in$ E, for each tuple s $\in$ S, if join cond matches, output.
    *   Cost: $M + (M \cdot p_E) \cdot N$ I/Os (if S is inner, scanned per tuple of E).
*   **Page-Oriented Nested Loops Join (L6, S20-S21):**
    *   For each page pe $\in$ E, for each page ps $\in$ S, join tuples.
    *   Cost: $M + M \cdot N$ I/Os (if E is outer). Choose smaller relation as outer.
*   **Block Nested Loops Join (BNLJ) (L6, S22-S29; L8, S34):**
    *   Use B buffer pages. Outer relation R1, inner R2.
    *   Read block of R1 (B-2 pages), scan all of R2 (1 page buffer), join.
    *   Cost: $M + (\lceil M / (B-2) \rceil \cdot N)$ I/Os (if E is outer, block size B-2).
    *   Choose smaller relation as outer to minimize blocks.
    *   *Example (L8, S34): E(1000p), S(500p), B=5. T1=E(10p), T2=S(250p). BNLJ on T1, T2. T1 outer: $10 + (\lceil 10/(5-2) \rceil \cdot 250) = 10 + (4 \cdot 250) = 1010$ I/Os.*
*   **Index Nested Loops Join (INLJ) (L6, S30-S32; L8, S36-S37):**
    *   For each tuple e $\in$ E (outer), use index on S (inner) to find matching s.
    *   Cost: $M + (M \cdot p_E) \cdot (\text{cost_to_find_match_in_S_using_index})$.
    *   Cost to find match:
        *   Hash index: ~1.2 I/Os (index) + 1 I/O (data, if clustered/key) or more (unclustered).
        *   B+ tree: 2-4 I/Os (index) + data retrieval cost.
    *   *Example (L8, S36-37): E(1000p, 100 recs/p), S(500p). Clustered hash index on S(CID). Selection $\sigma_{CID=7}(E)$ yields 1000 tuples (10 pages, T1). Join T1 with S. Cost for selection: 10 I/Os (clustered hash). Cost for join: For each of 1000 tuples from T1, probe S. Index: 1.2 I/Os. Data: 1 I/O (clustered). Total: $10 + 1000 \cdot (1.2+1) = 10 + 2200 = 2210$ I/Os. (Note: example in L8, S37 seems to use $M \cdot p_E$ for number of tuples from selection, which is 1000 here, not $M_{T1} \cdot p_{T1}$).
*   **Sort-Merge Join (SMJ) (L7, S15-S19):**
    1.  Sort E on join column: Cost $2M(\lceil \log_{B-1} \lceil M/B \rceil \rceil+1)$.
    2.  Sort S on join column: Cost $2N(\lceil \log_{B-1} \lceil N/B \rceil \rceil+1)$.
    3.  Merge sorted E and S: Cost M+N (best case) up to M*N (worst case, all join column values same).
    *   *Example (L8, S33): T1(10p), T2(250p), B=5. Sort T1: $2 \cdot 10 \cdot (\lceil\log_4 \lceil 10/5 \rceil\rceil+1) = 20 \cdot (\lceil\log_4 2\rceil+1) = 20 \cdot (1+1) = 40$ I/Os. Sort T2: $2 \cdot 250 \cdot (\lceil\log_4 \lceil 250/5 \rceil\rceil+1) = 500 \cdot (\lceil\log_4 50\rceil+1) = 500 \cdot (3+1) = 2000$ I/Os. Merge: $10+250 = 260$. Total: $40+2000+260 = 2300$ (excluding initial selection costs).*
*   **Hash Join (L7, S20-S26):**
    1.  Partitioning Phase: Hash E into $B-1$ partitions (E1..Ek), hash S into $B-1$ partitions (S1..Sk) using same hash fn on join col. Cost: 2(M+N) I/Os.
    2.  Probing Phase: For each partition Ei, build in-memory hash table (using h2 $\neq$ h1). Scan corresponding Si, probe hash table. Cost: M+N I/Os (if partitions fit).
    *   Total Cost: 3(M+N) I/Os (if no overflow).
    *   Memory requirement: Smallest partition should fit in B-2 pages, i.e., $M/(B-1) < B-2 \implies B \approx \sqrt{M}$.

#### **3.3. Other Operations (Lecture 7, 8)**

*   **External Merge Sort (General B-way) (L7, S3-S14):**
    *   N pages, B buffer pages.
    *   Pass 0: Create $\lceil N/B \rceil$ initial sorted runs of B pages each. Cost: 2N (read N, write N).
    *   Subsequent Passes: Merge B-1 runs at a time.
    *   Number of passes = $1 + \lceil \log_{B-1} (\lceil N/B \rceil) \rceil$.
    *   Total Cost = $2N \cdot (\text{num_passes})$.
    *   *Example (L13, E2): R(1000 pages), B=200. Pass 0: $\lceil 1000/200 \rceil = 5$ runs. Merge passes: $\lceil \log_{199} 5 \rceil = 1$. Total passes = 1+1=2. Cost = $2 \cdot 1000 \cdot 2 = 4000$ I/Os.*
*   **Projection ($\Pi_{attrs}(R)$) (L8, S3-S14):**
    *   **Sorting-based:**
        1.  Scan R, write out only desired attributes to E' (T pages). Cost M (read R) + T (write E').
        2.  Sort E' on all its attributes. Cost $2T(\text{sort_passes_for_T})$.
        3.  Scan sorted E', eliminate duplicates. Cost T (read E').
        *   Total cost: $M + 2T + 2T(\text{sort_passes_for_T})$.
        *   Improvement: Eliminate columns in Pass 0 of sort, eliminate duplicates during merge. (L8, S7, S10)
    *   **Hashing-based:**
        1.  Partitioning: Scan R, project attributes, hash to B-1 partitions. Cost M (read R) + T (write partitions).
        2.  Duplicate Elimination: For each partition, build in-memory hash table (h2), insert tuples, discard duplicates. Write out. Cost T (read partitions).
        *   Total cost: M + 2T. (L8, S14)
*   **Set Operations (Union, Intersection, Difference) (L8, S15-S16):**
    *   Sorting-based: Sort both relations, scan in parallel.
    *   Hashing-based: Partition R and S. For S-partitions, build hash table, probe with R-tuples.
*   **Aggregate Operations (L8, S17-S18):**
    *   Without grouping: Scan relation, maintain running info (SUM, COUNT, MIN, MAX).
    *   With grouping: Sort on grouping attributes, then scan. Or hash on grouping attributes.
    *   Index-only scan if index covers all attributes and grouping attributes are prefix of key.

#### **3.4. Query Optimization (Lecture 8, 9)**

*   **Goal:** Find a good (low-cost) evaluation plan. (L8, S19)
*   **Query Evaluation Plan:** Extended relational algebra tree with annotations (access method, join algorithm, pipelining/materialization). (L8, S20-S22)
*   **Estimating Costs & Result Sizes (L9, S4-S12):**
    *   Uses statistics from system catalog: `NTuples(R)`, `NPages(R)`, `NKeys(I)` (distinct key values for index I), `ILow(I)`, `IHigh(I)`.
    *   **Reduction Factor (RF) for `term`:**
        *   `column = value`: $1/\text{NKeys(I)}$ if index I on column. Else, typically 1/10 or use distinct value stats.
        *   `column1 = column2`: $1/\max(\text{NKeys(I1)}, \text{NKeys(I2)})$.
        *   `column > value`: $(\text{IHigh(I)} - \text{value}) / (\text{IHigh(I)} - \text{ILow(I)})$.
        *   Result size of selection $\sigma_{term}(R)$ $\approx$ `NTuples(R) * RF(term)`.
        *   Result size of $\sigma_{t1 \land t2}(R)$ $\approx$ `NTuples(R) * RF(t1) * RF(t2)` (assuming independence).
*   **Relational Algebra Equivalences (L9, S13-S17):**
    *   Cascading selections: $\sigma_{c1 \land c2}(R) \equiv \sigma_{c1}(\sigma_{c2}(R))$.
    *   Pushing selections: $\sigma_c(R \bowtie S) \equiv \sigma_c(R) \bowtie S$ (if c on R only).
    *   Pushing projections.
    *   Join commutativity & associativity.
*   **Enumerating Plans (System R approach) (L9, S18-S34):**
    *   **Single relation queries:** Consider different access paths (file scan, index scan, sorted index access, index-only). (L9, S19-S26)
    *   **Multi-relation queries (Joins):**
        *   Consider only **left-deep join trees**. (L9, S28-S29)
        *   N-pass dynamic programming approach:
            *   Pass 1: Best 1-relation plans.
            *   Pass k: Best k-relation plans by joining best (k-1)-relation plan with one more relation.
        *   Retain cheapest overall plan and cheapest plan for each "interesting order".
        *   Avoid cross-products if possible.
        *   GROUP BY/aggregates handled as final step.

---

### **Section 4: Distributed Databases (Lectures 10, 11)**

#### **4.1. Storing Data (Lecture 10)**

*   **Fragmentation (L10, S12, S14-S15):**
    *   **Horizontal:** Subset of rows. Defined by selection predicates. Reconstruct with UNION.
        *   *Example (L13, E4): `σbranch='Eroilor'(Accounts)` is a horizontal fragment.*
    *   **Vertical:** Subset of columns. Must be a good decomposition (lossless join, dependency preserving). Reconstruct with Natural Join.
*   **Replication (L10, S13, S16-S28):** Storing multiple copies.
    *   **Synchronous:** Changes propagated before transaction commits.
        *   Voting: Write majority, read enough to see current. (L10, S18)
        *   Read-any Write-all: Write all copies, read any copy. (L10, S19) (More common).
        *   Costs: Locking all copies, expensive commit protocol. (L10, S20)
    *   **Asynchronous:** Changes propagated periodically.
        *   Peer-to-peer: Multiple master copies. Needs conflict resolution. (L10, S22-S23)
        *   Primary Site: One master copy. Changes captured (log-based/procedural) and applied to secondary copies. (L10, S24-S28)

#### **4.2. Distributed Query Processing (Lecture 10)**

*   Costs: Disk I/O ($t_d$), Communication/Shipping ($t_s$). (L10, S29)
*   **Non-Join Queries (L10, S30-S33):**
    *   Horizontal fragments: Evaluate on fragments, union results. Aggregates might need partial computation then final combination.
    *   Vertical fragments: Reconstruct relation by joining fragments, then evaluate.
    *   Replication: Choose site with lowest processing + shipping cost.
*   **Join Queries (R at Site1, A at Site2) (L10, S34-S46):**
    *   **Fetch as needed (Page-NLJ with R outer, at Site1):** For each page of R, fetch all pages of A. Cost: $M_R \cdot t_d + M_R \cdot (M_A \cdot t_d + M_A \cdot t_s)$. (Simplified from L10, S35 which is tuple-oriented).
        *   *Tuple-oriented (L13, E3): $N_S \cdot t_d + (\text{tuples_S}) \cdot (\text{tuples_R_per_page} \cdot M_R \cdot (t_d + t_s))$ if R is inner and shipped per tuple of S. The exercise has S outer, R inner. Cost for S outer: $N_S \cdot t_d + (\text{tuples_S}) \cdot M_R \cdot (t_d + t_s)$. For L13, E3: $200 t_d + 2000 \cdot 1000 (t_d + t_s)$.*
    *   **Ship to one site:**
        *   Ship R to Site2, join at Site2: Cost $M_R(t_d+t_s) + M_R t_d + \text{JoinCost}(R,A)$ at Site2. (L10, S41)
        *   Ship A to Site1, join at Site1: Cost $M_A(t_d+t_s) + M_A t_d + \text{JoinCost}(R,A)$ at Site1. (L10, S42)
    *   **Semijoin:** Project R on join columns ($R'$), ship $R'$ to Site2. Join $R'$ with A to get reduction of A ($A'$). Ship $A'$ to Site1. Join R with $A'$. (L10, S43-44)
    *   **Bloomjoin:** Create bit-vector (Bloom filter) from join column of R. Ship to Site2. Filter A using bit-vector. Ship reduced A to Site1. Join. (L10, S45-46)

#### **4.3. Distributed Transaction Management (Lecture 11)**

*   **Distributed Concurrency Control (L11, S3-S14):**
    *   Lock Management: Centralized, Primary Copy, Fully Distributed.
    *   Distributed Deadlock Detection:
        *   Local waits-for graphs + global detection (Centralized, Hierarchical).
        *   Global deadlock can exist even if no local deadlocks. (L11, S6-S8)
        *   Phantom deadlocks due to propagation delays. (L11, S14)
*   **Distributed Recovery & Two-Phase Commit (2PC) (L11, S15-S28):**
    *   **Coordinator & Subordinates.**
    *   **Phases:**
        1.  **Voting Phase:**
            *   Coordinator sends `prepare` to all subordinates.
            *   Subordinate decides: force-writes `abort` or `prepare` log record, sends `no` or `yes` to coordinator.
        2.  **Termination Phase:**
            *   If all `yes`: Coordinator force-writes `commit`, sends `commit` to subordinates.
            *   Else (any `no` or timeout): Coordinator force-writes `abort`, sends `abort` to subordinates.
            *   Subordinates receive `commit`/`abort`: force-write `commit`/`abort` log, send `ack`, act accordingly.
            *   Coordinator writes `end` log after all `ack`s.
    *   **Log Records:** `prepare`, `commit`, `abort`, `end`, `ack` (implicitly by coordinator). `prepare` is specific to 2PC.
    *   **Restart after failure:** Depends on last log record for T (commit/abort, prepare, or none). (L11, S23-S24)
    *   **2PC with Presumed Abort:** Optimizations if T aborts or subtransactions are read-only. (L11, S27-S28)

---

### **Section 5: Data Encryption (Lecture 5)**

*   **Algorithm Example (L5, S34-S37) - Relevant for L13, E4:**
    1.  **Create character code table:** e.g., a=00, b=01, ..., z=25, space=26. Let $n$ be number of characters (e.g., 27).
    2.  **Key:** A secret string (e.g., "metallica"). Let its length be $L$.
    3.  **Divide message into blocks of length L.** (Or process character by character, repeating key).
    4.  **For each message character `Msg_char` at position `i` and corresponding key character `Key_char` (from repeating key at `i mod L`):**
        *   Get numeric value: `Msg_val`, `Key_val`.
        *   Encrypted value `Enc_val = (Msg_val + Key_val) $\bmod n$`.
        *   Convert `Enc_val` back to character.
    5.  **Decryption:** `Msg_val = (Enc_val - Key_val + n) $\bmod n$`.

---

This cheat sheet should cover the main theoretical aspects needed for exercises similar to those in Lecture 13. Remember to understand the concepts rather than just memorizing formulas. Good luck!
