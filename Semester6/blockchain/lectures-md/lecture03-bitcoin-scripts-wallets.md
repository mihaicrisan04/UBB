# Lecture 3 — Bitcoin Scripts & Wallets

## Bitcoin block & Tx structure

### Block header (80 bytes)
| field | size |
|---|---|
| version | 4 B |
| prev hash | 32 B |
| time | 4 B |
| bits | 4 B |
| nonce | 4 B |
| Tx root (Merkle) | 32 B |

Chain: `BH₁ ← BH₂ ← BH₃ …` linked by `prev` hash. Each block commits to its txs via the Merkle `Tx root`.

### Transaction structure
```
Tx = inputs[] + outputs[] + witnesses + locktime
```
- **Input**: `TxID (32B) | out-index (4B) | ScriptSig | seq`
- **Output (UTXO)**: `value (8B) | ScriptPK`
- **TxID = H(Tx excluding witnesses)** — that's what SegWit fixed
- **locktime**: earliest block # that can include this Tx
- `#BTC = value / 10⁸` (smallest unit = satoshi)

### Validating a spending Tx
For each input, a miner checks:
1. The combined program `ScriptSig | ScriptPK` evaluates to `true`
2. `(TxID, index)` is in the **current UTXO set** (not spent yet)
3. `Σ inputs.value ≥ Σ outputs.value` (difference = miner fee)

After Tx is mined, the spent UTXOs are removed from the UTXO set.

---

## Transaction types

### (1) P2PKH — pay to public-key hash
Alice pays Bob 5 BTC:
1. Bob generates `(pk_B, sk_B)`
2. Bob's address: `addr_B = H(pk_B)`
3. Alice posts Tx with output containing:
   ```
   ScriptPK: DUP HASH256 <addr_B> EQVERIFY CHECKSIG
   ```
4. When Bob spends:
   ```
   ScriptSig: <sig> <pk_B>   where  sig = Sign(sk_B, Tx without sigs)
   ```

**Why hash the pk?** Doesn't reveal pk until UTXO is spent (extra security against future pk attacks). Miner cannot tamper with `<addr_B>` — would invalidate Alice's sig.

### (2) P2SH — pay to script hash (pre-SegWit, 2017)
Payer specifies a **redeem script**. Bob's address = `H(redeem)`.

```
ScriptPK in UTXO:  HASH160 <H(redeem)> EQUAL
ScriptSig to spend: <sig₁> <sig₂> … <redeem script>
```

Miner verifies: (1) script hash matches, (2) script evaluates to true.

### (3) Multisig (t-of-n)
A redeem script for 2-of-3:
```
<2> <PK₁> <PK₂> <PK₃> <3> CHECKMULTISIG
```

Spend with: `<0> <sig₁> <sig₃> <redeem script>` (the `<0>` is a CHECKMULTISIG quirk).

### SegWit (2017)
Fixes **ECDSA malleability**: anyone could re-randomize sig → change TxID without invalidating it. SegWit moves sigs to a separate `witness` field so `TxID = H(Tx without witnesses)` is stable.

---

## Example use cases of multisig

### Co-signatory wallet (theft protection)
Funds at `addr = 2-of-2(PK_A, PK_S)`. Alice and a custody server both sign — theft of Alice's `sk_A` alone doesn't drain funds.

### Escrow service (Alice ↔ Bob ↔ Judge)
- Funds at `addr = 2-of-3(PK_A, PK_B, PK_J)`
- Backpack arrives → Alice + Bob sign → Bob paid
- Backpack lost → Alice + Judge sign → Alice refunded
- Both at fault → Judge releases split Tx

---

## Wallets

### Types
- **Cloud** (Coinbase): exchange holds sk — "not your keys, not your coins"
- **Laptop/phone** (Electrum, MetaMask): you hold sk
- **Hardware** (Ledger, Trezor): sk in secure element
- **Paper**: print sk
- **Brain**: memorize (bad)
- **Hybrid (non-custodial cloud)**: threshold signatures

### Hardware wallet basics
- Bolos OS (Ledger): each coin = an app
- PIN (up to 48 digits) unlocks
- Screen + buttons to confirm Tx (defeats malware on host)

### Hierarchical Deterministic (HD) wallets — BIP-39
**Goal**: 1 backup → infinite keys.

**Idea 1**: secret seed `k₀ ∈ {0,1}²⁵⁶`. Derive:
```
skᵢ = HMAC(k₀, i)
pkᵢ = g^skᵢ
```
`k₀` stored as **24 mnemonic words** (each = 11 bits, 24×11 = 264 bits + checksum). Words from a public 2048-word list (BIP-39).

⚠️ Problem: need `k₀` even just to check your balance.

**Idea 2 (used in HD wallets)**:
```
secret seed: k₀ ∈ {0,1}²⁵⁶
(k₁, k₂) = HMAC(k₀, "init")
balance seed: k_pub = (k₂, h = g^k₁)

skᵢ = k₁ + HMAC(k₂, i)
pkᵢ = g^skᵢ = h · g^HMAC(k₂, i)   ← computable from k_pub!
```
Result:
- `k_pub` on laptop/phone → generates unlinkable **addresses** for balance checking
- `k₀` only on hardware wallet → needed to spend

### SPV (Simplified Payment Verification)
Light wallet:
1. Download all block headers (~60 MB total, the full chain is ~366 GB)
2. Ask server for Tx of interest + Merkle proof to block header

**Problems**:
- Security: are these the real headers? Can server omit Tx? (Electrum: query 10 random servers.)
- Privacy: server learns which addresses are yours.

### Paper wallet
- Public address (QR + base58) on one side
- `sk` in cleartext on the other (with QR)
- Base58 = `a-zA-Z0-9` minus `{0, O, l, 1}` (visually ambiguous chars excluded)

---

## Exchanges: hot/cold storage
Coinbase design:
- **98 % of assets** in cold storage as `t-of-n` secret-sharing of `k₀`
- **2 %** in hot wallet for customer operations
- Hot wallet holds `(h, k₂)` to verify cold-storage balances without spend ability

**Problem**: can't prove ownership of cold-storage assets without unlocking them (audit / PoS participation).
**Solution**: **proxy keys** that prove ownership but cannot spend.

---

## Exam-relevant takeaways
- ScriptPK locks coins, ScriptSig unlocks them; miner runs `ScriptSig | ScriptPK`
- P2PKH = normal payment; P2SH = arbitrary spend conditions hashed to an address
- Multisig (2-of-3) is the basis of escrow & co-signatory wallets
- SegWit fixes malleability by moving signatures out of `TxID = H(Tx)`
- HD wallets: one seed `k₀` (24 words) → all keys; a balance-only seed `k_pub` exists for non-signing devices
- SPV needs only block headers + Merkle proofs
