# Lecture 6 — Incentives & Accountability: Proof-of-Stake

## Recap — security for SMR
- **Safety**: for any two clients i,j and times t,s: `LOG_t^i ⪯ LOG_s^j` or vice-versa (no double spend)
- **Liveness**: tx input to an honest replica at t ⇒ in every client's log by `t + T_conf` (no censorship)

## Recap — Nakamoto
- Sybil resistance: PoW
- Longest chain rule, k-deep confirmation
- Secure iff `β < ½`, mining rate λ small enough
- Dynamic availability ✓, no finality ✗

---

## Incentives in Bitcoin
- Block reward (currently **3.125 BTC**, halved every 210 000 blocks ≈ 4 years)
- Tx fees

### Does this *guarantee* honest behavior? NO.
**Selfish mining** (see optional slides): a miner with > 25 % hash power can publish blocks strategically to mine more than its "fair share".

### MEV (maximal extractable value)
Miner profit via inclusion / exclusion / reordering of txs.

**Example**: Miner A mines a k+1-deep block including `tx₁…tx₄` (total fees: 10 BTC). Miner C can:
1. Fork off A's chain just before A's block
2. Re-include `tx₁` themselves to claim 6 BTC of fees
3. Extend their fork → wins reorg if compute permits

**Consequences**:
- Miner A's block gets **reorged**
- Bob sees `tx₂` was confirmed, then disappears → **safety violated**, potential double-spend

> MEV gives rational miners a reason to break protocol — `β < ½` doesn't necessarily hold under economic rationality.

---

## From Bitcoin to Proof-of-Stake

| | 1982 BGP | 2008 Bitcoin | 2022 Eth PoS |
|---|---|---|---|
| Setting | permissioned | open | open |
| Sybil resistance | n/a | PoW | PoS (locked coins) |
| Carrot | n/a | block rewards | block rewards |
| Stick | n/a | none | **slashing** |
| Finality | yes | no | yes |
| Accountable safety | n/a | no | yes |

---

## Proof-of-Stake idea
- Nodes **lock (stake) their coins** to participate in consensus
- More stake → higher probability of being elected leader & larger vote weight
- If a node is caught misbehaving (e.g. signing two conflicting blocks at same height) → its **stake is burned** = *slashing*
- → Nodes are held **accountable** for actions (impossible in PoW where there's no locked deposit)

---

## A simple PBFT-style PoS protocol
- n validators each lock equal stake; quorum = `⌈2n/3⌉`
- Each epoch produces a candidate block + the predecessor finalizes when ≥ ⌈2n/3⌉ sign it
- Vote = signed pair (block, epoch)

### Why 2n/3?
Suppose two different blocks (`txs₄`, `txs₅`) both reach the 2n/3 quorum at the same epoch. Then:
```
|votes for txs₄| + |votes for txs₅| ≥ 2n/3 + 2n/3 = 4n/3
⇒  ≥ n/3 nodes voted twice
⇒  ≥ n/3 nodes are adversarial AND provably caught
```
- **Safety** if `< n/3` adversarial
- **Slashable evidence** of `≥ n/3` violators if safety ever breaks
- **Liveness** if `< n/3` crashed (need quorum of 2n/3 to make progress)

### Trade-offs of quorum size
| Quorum | Safety | Liveness |
|---|---|---|
| 2n/3 | < n/3 adversarial | < n/3 crashed |
| 7n/9 (larger) | < 5n/9 adversarial (better!) | need < 2n/9 crashed (worse!) |
| n (unanimous) | safe to adversary < n | any single crash kills liveness |

Larger quorum → more safety, less liveness. n/3 split is the standard sweet spot.

---

## Accountable Safety

In a protocol with **resilience n/3**:
- The protocol is **safe + live** if fewer than n/3 nodes are adversarial.

In a protocol with **accountable safety resilience n/3**:
- Safe if `< n/3` adversarial.
- If safety is *ever* violated, all observers can **provably identify ≥ n/3 protocol violators** (e.g. by their double-vote signatures).
- **No honest node is ever falsely accused.**

> **Examples**: simple PoS protocol above, **PBFT**, **Tendermint**, **HotStuff**, **Casper FFG**.

---

## Sybil resistance × consensus protocol matrix

|  | **PoW** | **PoS** |
|---|---|---|
| **Nakamoto (longest chain)** — dynamic availability | Bitcoin | Ouroboros |
| **PBFT-style (votes)** — finality + accountable safety | (rare/none) | Ethereum, simple PoS above |

Ethereum combines **both** in practice: a finality gadget (Casper FFG) layered on an available chain (LMD-GHOST).

---

## Exam-relevant takeaways
- PoS replaces PoW's energy stick with **slashing**
- Quorum `2n/3` is the magic number: safety < n/3 adversarial AND ≥ n/3 violators are *provably caught* on a safety break
- **Accountable safety** = "if safety breaks, ≥ n/3 are publicly identifiable as violators, with no false accusation"
- Choosing a larger quorum buys more safety but less liveness (you can't have both)
- MEV is the chief economic threat to honest behavior in any chain
