# Lecture 7 — Ethereum: Mechanics

## Motivation — Bitcoin's limitations
Bitcoin script can't:
- Maintain state across multi-stage contracts
- Enforce global rules on assets (e.g. rate limiting)
- Implement DNS-like contracts (where "once registered, no one else can register `google.com`")

NameCoin forked Bitcoin to add this. **Ethereum** generalizes: a blockchain that natively supports arbitrary contracts.

---

## Ethereum at a glance
- A world of DAPPs: ERC-20 coins, DeFi, DAOs, NFTs (ERC-721), insurance, …
- Two layers per node:
  - **Consensus layer (beacon chain)**: orders Txs
  - **Compute layer (execution chain / EVM)**: updates world state

```
[user] → [consensus layer: 12s slots × 32 = epoch (6.4 min)] → notify_new_payload → [EVM updates state]
```

- One **block proposer** chosen per 12-s slot
- Validators stake **32 ETH** (or use a staking pool like Lido)
- Issuance + tips + MEV ≈ 2.93 % APY on 32 ETH

---

## Bitcoin vs Ethereum as state machines
- Bitcoin: `F_bitcoin : S × I → S` where S = set of UTXO sets, transitions are trivial
- Ethereum: much richer `S` and `F` — every Tx can execute an arbitrary program

---

## Account types
World state = set of accounts indexed by **20-byte address**.

### (1) EOA — externally-owned account
- Controlled by an **ECDSA key pair** `(pk, sk)`
- `address = H(pk)`
- *Since May 2025*: EOAs can also be controlled by code (EIP-7702)

### (2) Contract account
- Controlled by **code**, set at creation time, immutable
- `address = H(creatorAddr, creatorNonce)`

### Account data
| field | EOA | Contract |
|---|---|---|
| `address` (computed) | H(pk) | H(creator, nonce) |
| `code` | ⊥ (or address) | CodeHash |
| `storage root` | ⊥ | StorageRoot |
| `balance` (Wei) | balance | balance |
| `nonce` | nonce | nonce |

> `nonce` = `#Tx_sent + #accounts_created` — anti-replay mechanism.

### Account state (persistent storage)
Each contract owns a virtual array `S[0]…S[2²⁵⁶−1]`, 32 bytes per cell, zero-initialized.

Stored as a **Merkle Patricia Tree** keyed by storage slot. `StorageRoot` is just the Merkle root.

> 1 wei = `10⁻¹⁸` ETH. 1 Gwei = `10⁻⁹` ETH.

---

## Transactions
Signed by an EOA initiator (or, since EIP-7702, by code).

| field | meaning |
|---|---|
| `to` | 20-byte target address (0 → create new contract) |
| `from` | initiator address |
| `value` | # Wei sent with Tx |
| `data` | function selector + ABI-encoded args |
| `gasLimit` | max gas allowed for this Tx |
| `maxFee` | maximum gas price |
| `maxPriorityFee` | tip to block proposer |
| `nonce` | must match sender's current nonce (anti-replay) |
| `chain_id` | which chain (mainnet, sepolia, …) |
| `[signature]` | EOA sig over Tx |

### Tx types
- `owned → owned`: transfer ETH between users
- `owned → contract`: call contract with ETH + data
- `contract → owned` or `contract → contract`: **messages** (virtual Txs initiated by a contract). No signature — contracts don't sign.

**Composability**: a single user Tx can ripple through many contracts via messages.

---

## Block structure
Block proposer:
1. Picks `n` Txs from mempool
2. For `i = 1..n`: executes Tx_i sequentially, mutating world state
3. Records updated world state hash in the block

Other validators **re-execute** all Txs to verify the block, then sign. Enough sigs → epoch finalizes.

### Block header (simplified)
| field | description |
|---|---|
| consensus data | proposer ID, parent hash, votes, … |
| gas beneficiary | where fees go |
| world state root | Merkle Patricia Tree of all accounts |
| Tx root | Merkle hash of all Txs in block |
| Tx receipt root | Merkle hash of all log messages |
| gas used | for base-fee adjustment |

### Sizes (Oct 2025)
- Single-node DB: ~1.4 TB
- Full archival chain: ~24 TB

---

## The EVM

### Architecture
- **Stack machine** like Bitcoin, but **with JUMP** (Turing-complete subject to gas limit)
- Max stack depth = **1024**
- Block proposer keeps gas fee even if program aborts
- Contracts can `CREATE` other contracts and `CALL` other contracts (new execution frame on CALL)

### Memory types per contract
| type | opcodes | cost | lifetime |
|---|---|---|---|
| Persistent storage | SLOAD / SSTORE | expensive | persistent on-chain |
| Volatile memory | MLOAD / MSTORE | very cheap (3 gas) | one execution frame |
| Transient memory | TLOAD / TSTORE | cheap (100 gas) | one Tx, persists across frames |
| Log (events) | LOG0…LOG4 | cheap | in tx receipt (cheap, off-chain readable) |
| Calldata | CALLDATALOAD | 16 gas per non-zero byte | this Tx only, read-only |

### Storage costs (SSTORE)
| transition | cost |
|---|---|
| `zero → non-zero` | 20 000 gas |
| `non-zero → non-zero` (cold slot) | 5 000 gas |
| `non-zero → zero` | refund 15 000 gas (incentivizes deleting state) |

Others:
- `CREATE`: `32 000 + 200 × code_size`
- `SELFDESTRUCT addr`: 5 000 gas + 25k if addr empty

---

## Gas calculation (EIP-1559, since 8/2021)

### Goals
- Users incentivized to bid their true utility
- Block proposers disincentivized from fake-Txs
- Disincentivize off-chain agreements

### Mechanics
Every block has a **baseFee** — the minimum gas price all Txs must pay.

```
baseFee_next = baseFee_cur · (1 ± 12.5 %)
              based on whether last block was above/below 22.5M gas target
              (max block size = 45M gas)
```

Every Tx specifies three fields:
- `gasLimit` — max gas allowed
- `maxFee` — maximum gas price user willing to pay
- `maxPriorityFee` — tip to block proposer

```
gasPrice = min(maxFee, baseFee + maxPriorityFee)
```

### Settlement
After Tx executes, `gasUsed` known:
1. If `gasPrice < baseFee` → abort
2. If `gasLimit × gasPrice > sender.balance` → abort
3. Deduct `gasLimit × gasPrice` from sender
4. Run Tx, deducting per-instruction; if gas runs out → abort, proposer keeps `gasLimit × gasPrice`
5. Refund unused gas to sender
6. Of the actually-consumed fee:
   - **`gasUsed × baseFee` → 🔥 BURNED**
   - **`gasUsed × (gasPrice − baseFee)` → block proposer (tip)**

### Why burn baseFee?
If baseFee went to the block proposer, in periods of low Tx volume the proposer would offer **off-chain refunds** of baseFee to attract fake Txs → undermines fee mechanism. Burning kills this incentive.

### Side effect
- Issuance > burn → ETH inflates
- Issuance < burn → ETH deflates

---

## Exam-relevant takeaways
- Two account types: **EOA** (key pair) and **contract** (code, no key)
- A user Tx can trigger many **messages** between contracts (composability)
- Three storage classes: **persistent (storage), volatile (memory), transient** — picking the right one matters for gas
- **EIP-1559** formula: `gasPrice = min(maxFee, baseFee + maxPriorityFee)`; baseFee **burned**, priority fee **to proposer**
- `nonce` = anti-replay; `chain_id` = anti-cross-chain replay
- Validators **re-execute** every Tx to validate a block — no shortcuts
