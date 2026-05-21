# Lecture 4 — Fundamentals of Consensus

## Setting up the problem

### Adversary models (in order of strength)
- **Crash fault**: adversarial nodes simply stop
- **Omission fault**: adversary selectively drops messages
- **Byzantine fault**: adversary deviates arbitrarily from the protocol

> Bound the adversary by `f` (number of corruptable nodes out of `n`). Typical bounds: `f < n/2`, `f < n/3`, `f < n`.

### Communication assumption
- Authenticated via signatures (PKI setup)
- Adversary **cannot forge** signatures of honest nodes (otherwise it could simulate them)
- Alternative to PKI: proof-of-work

### Network models
- **Synchronous**: adversary must deliver any message within Δ rounds (Δ known)
- **Asynchronous**: adversary can delay messages arbitrarily, but must eventually deliver

> Honest nodes follow the protocol. The adversary "runs the network" subject to the model's constraints.

---

## Byzantine Generals Problem (BGP) — Lamport, Shostak, Pease (1982)
- `n` generals, one is the **commander**
- Some loyal (honest), some traitors (incl. possibly commander)
- Commander sends `attack` or `retreat` to each general
- All take an action eventually

**Goals:**
- **Agreement**: no two loyal generals take different actions
- **Validity**: if commander is loyal, all loyal generals follow commander's order
- **Termination**: all loyal generals eventually act

> Maps to "nodes ↔ generals", "leader ↔ commander", "honest ↔ loyal", "adversary ↔ traitor".

---

## Byzantine Broadcast (BB)
Same problem, modern naming.
- n nodes, one is leader with input `v ∈ {0,1}`
- Network synchronous, ≤ f nodes adversarial

**Goals:**
- **Agreement**: no two honest nodes output different values — *prevents double-spending*
- **Validity**: if leader is honest, all honest output the leader's value
- **Termination**: all honest nodes eventually output — *prevents censorship*

Agreement must hold **even when the leader is adversarial**.

---

## Strawman protocols (Δ = 1)

### Strawman I (broadcast-only)
Time 0: leader broadcasts `<v: A>`. Time 1: each node decides based on what it received.

- ✅ Works if leader is honest (validity).
- ❌ Adversarial leader sends `<1:A>` to B and `<0:A>` to C → B and C decide different values → **agreement violated**.

### Strawman II (echo once)
Time 0: leader sends `<v: A>`. Time 1: each node echoes `<v': A, i>` to others. Time 2: decide based on accumulated values `Vᵢ`.

`choice(V)`: if `V = {v}` return v, else return 0.

- ✅ When n=3 and leader is adversarial: after echoing, both honest nodes see `V = {0,1}` → both decide 0 → agreement holds.
- ✅ When leader is honest and one node is adversarial: invalid signature chains (e.g. `<0: C, C>` missing leader's sig) are rejected → honest node keeps `V = {1}`.

---

## Dolev-Strong protocol (1983)
**Generalization of Strawman II to f+1 rounds.**

Time 0: leader broadcasts `<v: A>`.
Time t = 1…f: each node, on receiving `<v': A, i₁,…,i_{t-1}>` with `t-1` distinct signers ≠ self and `v' ∉ Vᵢ`:
- add v' to Vᵢ
- broadcast `<v': A, i₁,…,i_{t-1}, self>`

Time f+1: accept `<v': A, i₁,…,i_f>` (f signatures), then decide `choice(Vᵢ)`.

### Security
**Theorem (Dolev-Strong, 1983)**: For any `f < n`, Dolev-Strong with `f+1` rounds satisfies agreement, validity, termination in a synchronous network.

**Converse**: any deterministic Byzantine-resilient protocol for synchronous BB needs **at least f+1 rounds** in the worst case.

> Intuition: in the worst case, the adversary delays committing to a value until the last possible round, so honest nodes need one extra round to relay the news.

---

## State Machine Replication (SMR)
Generalize single-shot BB to a **continuous log of transactions**.

- **Centralized bank** view: `(tx_i, st_{i-1}) → (y_i, st_i)` — every Tx updates state.
- **Blockchain (SMR)** view: an ever-growing, linearly-ordered **log** of Tx replicated across nodes.

### Two roles
- **Replicas** (miners/validators): receive Tx, run SMR, produce the log.
- **Clients** (wallets, dapps): ask replicas for the log; trust **majority of replicas**.

> Clients do NOT participate in the protocol. They just query and accept the majority answer. → wallet learns the correct log if > ½ of replicas are honest.

### Concatenation & prefix
- `A‖B` = sequence concatenation
- `A ⪯ B`: A is a prefix of B, i.e. ∃ C with `B = A‖C`
- Two sequences are **consistent** if `A ⪯ B` or `B ⪯ A`

### Security of SMR
Let `LOG_t^i` be the log output by client i at time t. A secure SMR satisfies:

- **Safety (Consistency)**: for any i,j,t,s: `LOG_t^i ⪯ LOG_s^j` or `LOG_s^j ⪯ LOG_t^i` (logs are consistent). → *No double spend.*
- **Liveness**: ∃ constant `T_conf` such that if a Tx is input to an honest replica at time t, then for all clients i and times `s ≥ t + T_conf`: `tx ∈ LOG_s^i`. → *No censorship.*

### Why safety matters — double spend
Eve has a UTXO; signs `tx₁` (pay Alice the car vendor) and `tx₂` (pay Bob the car vendor). If at time t₁ Alice sees `tx₁` confirmed but at t₂ Bob sees `tx₂` confirmed and `tx₁` is *not* in Bob's log → both deliver cars → **safety violated**.

### SMR vs BB
| | BB | SMR |
|---|---|---|
| Single-shot vs multi-shot | single | multi |
| Who outputs the decision | the nodes themselves | external clients |
| Replicas talk to clients? | no | yes |

---

## Exam-relevant takeaways
- **Agreement = no double spend.** **Termination/liveness = no censorship.** This mapping is the heart of the lecture.
- **Dolev-Strong: f+1 rounds, works for any f < n** in synchronous nets with signatures (lower bound is also f+1).
- Byzantine adversaries can deviate arbitrarily; with **signatures, they can't impersonate honest nodes**.
- SMR's safety = "for any two clients and times, their logs are prefix-consistent". This is the formal way to state "no fork that confuses honest users".
- Clients = wallets — they query replicas and trust the majority answer, they don't run consensus.
