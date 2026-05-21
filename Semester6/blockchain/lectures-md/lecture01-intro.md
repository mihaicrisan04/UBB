# Lecture 1 — Introduction & Cryptography Background

## Why blockchains?
A blockchain provides **coordination between many parties when there is no single trusted party**. If a trusted party exists, you don't need a blockchain.

**Timeline:**
- 2009 — **Bitcoin**: public append-only data structure, fixed supply asset
- 2015 — **Ethereum**: blockchain *computer*, composable DAPPs
- 2017+ — DeFi growth, DAOs, NFTs, stablecoins

## Architecture (layers)
```
┌───────────────────────────────────────┐
│  User-facing tools (cloud servers)    │
├───────────────────────────────────────┤
│  Applications (DAPPs, smart contracts)│
├───────────────────────────────────────┤
│  Execution engine (blockchain comp.)  │
├───────────────────────────────────────┤
│  Sequencer: orders transactions       │
├───────────────────────────────────────┤
│  Data availability / Consensus layer  │
└───────────────────────────────────────┘
```

## Consensus layer (informal properties)
A public append-only data structure with:
- **Persistence**: once added, data can never be removed
- **Safety**: all honest participants have the same data
- **Liveness**: honest participants can add new transactions
- **Permissionless**: anyone can add data (no authentication)

> All achieved by **replication**.

## Why is consensus hard?
- **Network delays** (Δ-delay) can shuffle Tx order between nodes
- **Network partitions** split honest nodes
- **Crashes** drop messages
- **Malice** (Byzantine): arbitrary deviation from protocol

## Course pillars
Cryptography · Economics · Distributed Systems

---

## Cryptographic background

### 1) Hash functions
`H: M → T` with `|M| ≫ |T|`, e.g. `SHA256: x → {0,1}²⁵⁶` (32 bytes).

- **Collision**: pair `x ≠ y` with `H(x) = H(y)`
- **Collision-resistant (CRF)**: "hard" to find any collision

**Binding commitment**: Alice posts `h = H(m)`. Later she reveals `m`. Since H is CRF, she can't substitute another `m' ≠ m`. (Note: hash is NOT hiding.)

### 2) Merkle trees (1989)
Goal: commit to list `S = (m₁,…,mₙ)` with a short hash `h`, and later prove `S[i] = mᵢ` with a `log₂ n` proof.

```
        h
       / \
      H   H
     /\   /\
    H  H H  H
    | | | | | | | |
    m₁m₂m₃m₄m₅m₆m₇m₈
```

**Proof of `S[4] = m₄`** = `(m₃, y₁, y₆)`:
```
y₂ = H(m₃, m₄)
y₅ = H(y₁, y₂)
h' = H(y₅, y₆)
accept iff h = h'
```

**Use in blockchains**: a block header contains just the Merkle root of all Txs → keeps chain small. Light clients verify Tx inclusion with a Merkle proof.

### 3) Proof of Work
Computationally hard puzzle: given `x`, find `y` s.t. `H(x, y) < 2ⁿ/D` (D = difficulty).
- **Asymmetric**: `Ω(D)` work to solve, `O(1)` to verify
- **Parallelizable**: more machines = faster solution
- Bitcoin uses `H(x,y) = SHA256(SHA256(x.y))`

### 4) Digital signatures
How to authorize a Tx. Each user has key pair `(pk, sk)`:
- `Sign(sk, m) → σ`
- `Verify(pk, m, σ) → accept/reject`

Adversary without sk cannot forge signatures. Used to prove "I authorized this transfer".

---

## Exam-relevant takeaways
- Blockchain = append-only public ledger via replication + crypto
- **Safety** ≈ agreement (no double-spend), **Liveness** ≈ no censorship
- Merkle proofs are `O(log n)` and the foundation of SPV light clients
- PoW is the sybil-resistance primitive for permissionless consensus
- Signatures + PKI assumed everywhere — adversary cannot impersonate honest nodes
