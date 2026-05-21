# Lecture 5 — Nakamoto Consensus (Bitcoin)

## Open vs permissioned consensus

### Sybil attack
Adversary spawns many fake nodes to outnumber honest ones in vote-based protocols (Dolev-Strong, PBFT-style). Open consensus needs **Sybil resistance**.

### Sybil resistance mechanisms
- **Proof-of-Work** — voting power ∝ compute (Bitcoin)
- **Proof-of-Stake** — voting power ∝ locked coins (Ethereum)
- **Proof-of-Space** — voting power ∝ disk

## Nakamoto's idea (Bitcoin, 2008)
- **Sybil resistance** via PoW: a block is valid only if `H(block) < 2ⁿ/D`
- **Fork choice rule**: nodes follow the **longest (heaviest) chain**
- **k-deep confirmation rule**: a Tx is *confirmed* once buried under k blocks (Bitcoin typically uses k=6)
- **Block reward + tx fees** incentivize honest mining

### Properties
- ✅ Permissionless, open participation
- ✅ Sybil-resistant (need actual compute)
- ✅ **Dynamic availability** (liveness under varying participation — nodes can join/leave)
- ❌ **No finality** — blocks are only *confirmed* with error probability ε(k), never fully finalized
- ❌ Loses safety under **asynchrony**
- ❌ **No accountable safety** — miners don't lock up anything, can't be slashed

---

## Nakamoto's private attack (β ≥ ½)
The classic safety attack. β = adversary fraction of mining power.

```
                                    Bob   adv    Alice
                                    sees   releases checks
                                    tx₂    secret   tx₁
t₀=0   t₁    t₂   t₃   t₄   t₅ t₆        t₇
       |     |    |    |    |  |          |
honest chain:       [tx₂][tx₄][tx₆]  ← rate λ_h
                    ↑ Bob's confirmed Tx (k-deep)
secret adv chain:   [tx₁][tx₃][tx₅][--]   ← rate λ_a
                                  ↑ released later, becomes longest chain
```

1. Adversary spends UTXO to Bob in `tx₂` on honest chain.
2. Adversary secretly mines a **competing chain** that includes `tx₁` (spending same UTXO to itself/Alice).
3. Bob sees `tx₂` go k-deep → ships goods.
4. Adversary releases secret chain when ≥ honest chain length.
5. By longest-chain rule, all nodes switch → `tx₂` **reorged** → Bob loses money. **Double spend.**

**Result**: attack succeeds w.h.p. iff `λ_a ≥ λ_h`, i.e. `β ≥ ½`.

### Forking lowers the practical bound
With network delay Δ, **honest miners fork against themselves** (two honest blocks at the same height). Honest chain effectively grows at rate `(1−β)/2`, while adversary's secret chain grows at full `β`.

→ adversary succeeds when `β ≥ (1−β)/2 ⇔ β ≥ 1/3`.

**Fix**: reduce mining rate λ. That's why Bitcoin has 10-min blocks, not 1-sec blocks.

---

## Security theorem
For `β < 1/2`, ∃ a small-enough mining rate `λ_safe(Δ, β)` such that Bitcoin satisfies safety AND liveness under Δ-synchronous network, except with error probability:

```
ε ≤ (2 + 2·√((1−β)/β)) · (4β(1−β))^k     ← exponentially small in k
```

> "Confirmation" instead of "finalization" because there's always some `ε(k) > 0`.

### Reminder — SMR security (recap)
- **Safety**: `ch_t^i ⪯ ch_s^j` or vice-versa (chains consistent)
- **Liveness**: tx submitted at time t → in confirmed chain by `t + T_conf`

---

## Proof sketch (optional)

**Loner block**: honest block such that no other honest block is mined within Δ time of it (no forking around it).

**Lemma**: for any `s > t`, the length of the shortest "longest chain" satisfies:
```
L(s) − L(t) ≥ # loners mined in (t+Δ, s−Δ]
```

**Pivot block**: a loner s.t. in every interval covering its mining time, more loners are mined than adversarial blocks.

**Theorem (existence)**: if `β < 1/2`, with small enough λ, any time interval of length T contains a pivot except with probability `e^(−Ω(√T))`.

**Theorem (pivot stickiness)**: a pivot block is on every longest chain held by any client at all later times — otherwise the adversary would need to outpace the loners in the interval, contradiction.

**Safety**: any two clients' k-deep chains share a pivot in the last k blocks, except with probability `e^(−Ω(√k))` → chains are consistent.

**Liveness**: pivots are mined by honest miners, include all submitted txs, and persist → liveness holds.

---

## Is Bitcoin the endgame? No.
- Bitcoin secure only under **synchrony** → loses safety in asynchrony
- **No finality** — confirmation has error ε(k)
- **Energy consumption** — PoW burns electricity
- **No accountability** — when β ≥ ½, no one is provably guilty

### What's next?
**Proof-of-Stake** can deliver low-energy consensus with **finality** and **accountable safety** — via slashing of locked stake.

| | PoW + Nakamoto | PoS-based (e.g. PBFT-style) |
|---|---|---|
| Sybil resistance | mining puzzle | locked coins |
| Block rewards | carrot | carrot |
| Finality | none | yes |
| Accountable safety | no | yes (slashing) |

---

## Do entities ever have ≥ 50 % hash power?
Ghash.IO had > 50 % in 2014 — voluntarily gave up. Why no visible attacks?
- "Miners care about price" — weak argument
- They *could* short the chain for profit — but then no more block rewards
- → No guarantees for the future

## MEV (maximal extractable value)
Miner profit by including / excluding / re-ordering Txs within its block. Examples: front-running, sandwich attacks. **MEV gives miners incentive to violate the protocol** → reorgs, double spends. The `β < ½` assumption may not hold rationally.

---

## Exam-relevant takeaways
- **β = 1/2** in theory, but **β = 1/3** in practice due to forking → why Bitcoin uses 10-min blocks
- **Private attack** is the canonical safety-break attack — adversary mines a secret longer chain
- Confirmation error decays **exponentially in k** (number of confirmation blocks)
- Bitcoin = `PoW + longest-chain + k-deep` → permissionless, dynamic availability, no finality
- MEV is an *incentive* problem, not a cryptographic one — pure protocols can't fix it
