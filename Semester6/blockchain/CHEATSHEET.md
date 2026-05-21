# Blockchain Smart Contracts — Exam Cheatsheet

> Open book. ~50 min. A few theory questions + extend a Solidity contract.

---

## 1. Bitcoin essentials

### Block & Tx structure
- **Block header** (80 bytes): version, prev hash, time, bits, nonce, Tx-root (Merkle).
- **Tx** = inputs[] + outputs[] + witnesses + locktime.
- **Input**: TxID (32B) + out-index (4B) + ScriptSig + seq.
- **Output (UTXO)**: value (8B) + ScriptPK.
- **TxID = H(Tx excluding witnesses)** (SegWit fixes ECDSA malleability).

### Validating a spending Tx
A miner checks for each input:
1. `ScriptSig | ScriptPK` evaluates to `true` (combined script returns true).
2. `(TxID, index)` is in the **current UTXO set** (not already spent).
3. `sum(inputs) ≥ sum(outputs)` (difference = miner fee).

### Transaction types

| Type | ScriptPK | Use |
|---|---|---|
| **P2PKH** (pay-to-pubkey-hash) | `DUP HASH256 <addr> EQVERIFY CHECKSIG` | normal payment |
| **P2SH** (pay-to-script-hash) | `HASH160 <H(redeem)> EQUAL` | complex conditions, e.g. multisig |
| **Multisig (t-of-n)** | redeem: `<t> <PK1>..<PKn> <n> CHECKMULTISIG` | escrow, co-signatory wallets |

### Wallets (managing SKs)

- **HD wallet (BIP-39)**: seed `k₀ ∈ {0,1}²⁵⁶` → 24 mnemonic words (each = 11 bits, 24×11=268 bits).
- **Idea 1**: `skᵢ = HMAC(k₀, i)`, `pkᵢ = g^skᵢ` — but needs k₀ even to check balance.
- **Idea 2 (HD wallets)**: split into `k₁,k₂ = HMAC(k₀, "init")`. Public seed `k_pub = (k₂, g^k₁)` can generate **addresses only** (for balance checking), `k₀` only on hardware wallet for signing.
- **Hot/cold storage**: exchange keeps ~98% in cold (t-of-n secret sharing), ~2% in hot wallet.

### SPV (Simplified Payment Verification)
Light wallet: download all **block headers** (~60 MB), ask server for Tx + Merkle proof. Privacy concern: server learns your addresses.

---

## 2. Consensus

### The setting
Nodes ↔ network. Adversary controls up to `f` nodes (Byzantine = arbitrary behavior). Cryptographic signatures + PKI assumed.

- **Synchronous network**: every message delivered within Δ rounds (Δ known).
- **Asynchronous network**: arbitrary finite delay.

### Byzantine Generals / Byzantine Broadcast (BB)

Leader has input value v. Properties:
- **Agreement**: no two honest nodes output different values.
- **Validity**: if leader is honest, all honest nodes output v.
- **Termination**: all honest nodes eventually output.

> Agreement = "no double spend". Termination = "no censorship".

### Dolev-Strong (1983)
For any `f < n`, solves BB in **f+1 rounds** in a synchronous network with signatures.

Idea: forward signed values with **signature chains** `<v: A, i₁, …, i_{t-1}>`. After f+1 rounds, if `|V_i| = {v}` decide v; else decide 0.

> **Lower bound:** any deterministic protocol resilient to f Byzantine faults needs ≥ f+1 rounds.

### State Machine Replication (SMR)
Generalization of BB to a **continuous log** of transactions.

- **Replicas** run SMR, **clients** (wallets) ask replicas for the log.
- Client trusts the **majority answer** from replicas.

**Safety (Consistency)**: for any clients i,j and times t,s either `LOG_t^i ⪯ LOG_s^j` or vice-versa (logs are consistent — one is a prefix of the other).

**Liveness**: ∃ `T_conf` s.t. if tx is input to an honest replica at time t, then for all clients i and times `s ≥ t + T_conf`, `tx ∈ LOG_s^i`.

> Safety = no double spend. Liveness = no censorship.

---

## 3. Nakamoto consensus (Bitcoin)

- **Sybil resistance**: PoW (mining puzzle).
- **Fork choice**: longest (heaviest) chain.
- **k-deep confirmation rule**: Tx confirmed once buried under k blocks.

### Security
With adversarial fraction β of mining power and synchronous network:
- **Theory:** safe & live iff `β < 1/2` (mining rate λ small enough).
- **Effective in practice:** forking from network delay cuts honest rate to ~`(1−β)/2`, so adversary wins if `β ≥ 1/3` unless λ is reduced (→ Bitcoin's 10-min blocks).
- **Error probability of k-deep rule** decays exponentially in k: `ε ≤ (2 + 2√((1-β)/β)) · (4β(1-β))^k`.

### Nakamoto's private attack (β ≥ 1/2)
Adversary mines a **secret chain** in parallel; once their chain ≥ honest chain, they release it → honest fork is orphaned → confirmed Tx reorged → **double spend**.

### Properties
- **Sybil resistance** ✓, **dynamic availability** ✓ (liveness under changing participation).
- **No finality**: blocks only *confirmed* with error ε(k), not finalized.
- **Loses safety in asynchrony**.
- **No accountability**: nobody can be "slashed" — miners don't lock up coins.

### MEV (maximal extractable value)
Miner profit from including / excluding / reordering txs. Gives miners an incentive to **violate the protocol** (reorg for fees) — `β < 1/2` doesn't necessarily hold rationally.

---

## 4. Proof-of-Stake (Ethereum)

### Idea
Nodes **lock up (stake)** coins to participate. Leader probability ∝ stake. Misbehavior → **slashing** (burn locked coins). This gives **accountable safety**.

### Simple PBFT-style PoS
Each epoch needs **≥ 2n/3 votes** for a block to finalize (quorum).
- Safe if `< n/3` adversarial. Live if `< n/3` crashed.
- If safety violated (two finalized conflicting blocks), at least `n/3` voted twice → **provably slashable**.
- Quorum `7/9` → safety `< 5n/9` but loses liveness if `> 2n/9` crash.

### Accountable safety
If safety is violated, **all observers can provably identify ≥ n/3 protocol violators**. No honest node is ever falsely accused.

### Ethereum PoS specifics
- Slot every **12 s**, epoch = 32 slots (≈ 6.4 min).
- One block proposer per slot, chosen from validator set.
- Validator stake: **32 ETH** (or Lido).
- Issuance + tips (priority fee) + MEV ≈ 2.9 % APY on 32 ETH.

### Bitcoin vs Eth-PoS

| | Bitcoin (PoW + Nakamoto) | Eth-PoS (PBFT-style) |
|---|---|---|
| Sybil resistance | PoW | PoS |
| Finality | No (confirmation only) | Yes |
| Accountable safety | No | Yes (slashing) |
| Dynamic availability | Yes | Limited (needs quorum) |
| Energy | High | Low |

---

## 5. Ethereum mechanics

### Accounts
World state = set of accounts indexed by 20-byte address. Two kinds:
- **EOA** (externally owned): `address = H(pk)`, controlled by ECDSA key.
- **Contract**: `address = H(creatorAddr, creatorNonce)`, controlled by **code** set at deploy.

Each account has: `address, code, storageRoot, balance, nonce`.

### Transactions
Signed by the EOA initiator. Fields:
- `to` (0 → create contract), `from`, `value` (Wei), `data` (function call + args), `nonce` (anti-replay), `chain_id`.
- Gas: `gasLimit, maxFee, maxPriorityFee` (EIP-1559).

### EVM
Stack machine (max depth 1024) + JUMP. **Three memory types per contract**:

| Type | Opcodes | Cost | Lifetime |
|---|---|---|---|
| **Storage** (on-chain) | SLOAD/SSTORE | expensive (20k zero→nonzero, 5k nonzero→nonzero) | persistent |
| **Volatile memory** | MLOAD/MSTORE | very cheap (3 gas) | one execution frame |
| **Transient memory** | TLOAD/TSTORE | cheap (100 gas) | one Tx, persists across frames |

Logs: `LOG0..LOG4`. Calldata: read-only, 16 gas / non-zero byte.

### Gas (EIP-1559)
```
gasPrice = min(maxFee, baseFee + maxPriorityFee)
gasUsed × baseFee          → BURNED 🔥
gasUsed × (gasPrice − baseFee) → block proposer (tip)
```
- `baseFee` adjusts ±12.5% per block: target 22.5M gas, max 45M.
- Burning the baseFee disincentivizes off-chain refund agreements between proposers and users.

### Block content
- Consensus data (proposer, parent hash, votes…).
- World-state root (Merkle Patricia Tree over all accounts).
- Tx root, Tx receipt root.
- Gas used.

---

## 6. Solidity — language cheatsheet

### Contract skeleton
```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import "@openzeppelin/contracts/access/Ownable.sol";

interface IERC20 { function transfer(address,uint256) external returns(bool); }

contract Foo is Ownable {
    constructor() Ownable(msg.sender) {}
}
```

### Types
- **Value**: `uint256, address, bool, bytes32, intN`. `address.balance`, `address.send/transfer/call`.
- **Reference**: `struct, array[], bytes, string, mapping(K=>V)`.
- Strings comparable only via hash: `keccak256(bytes(a)) == keccak256(bytes(b))`.

### Visibility & state mutability

| keyword | meaning |
|---|---|
| `external` | called only from outside; args from **calldata** |
| `public` | external + internal |
| `internal` | this contract + derived |
| `private` | this contract only |
| `view` | reads storage, no writes |
| `pure` | doesn't touch storage |
| `payable` | can receive ETH (msg.value) |

### Data locations (function params)
- `calldata` — read-only, cheapest, only `external`.
- `memory` — copied to RAM, mutable.
- `storage` — reference to state, mutable.

### Globals
```
msg.sender, msg.value, msg.data, msg.sig
tx.origin, tx.gasprice
block.number, block.timestamp, block.coinbase
keccak256(bytes), sha256(bytes)
abi.encode/encodePacked/encodeWithSelector
require(cond, "msg"), assert(cond), revert("msg")
```
> `tx.origin` = original EOA, `msg.sender` = direct caller. Never use `tx.origin` for auth.

### Modifiers
```solidity
modifier onlyOwner { require(msg.sender == owner, "not owner"); _; }
modifier nonReentrant { require(!locked); locked = true; _; locked = false; }
```

### Inheritance & libraries
```solidity
contract A is B, C { ... }    // linear inheritance (C3)
library Search { function indexOf(...) {...} }   // code runs in caller's context
```

### Calling other contracts
```solidity
ERC20 t = ERC20(_addr);
bool ok = t.transfer(to, value);            // ABI handled by compiler
(bool ok, ) = _addr.call{value: 1 ether}(""); // low-level, fallback
```

### ABI encoding
- Calldata first 4 bytes = function selector = `bytes4(keccak256("transfer(address,uint256)"))`.
- Special fallbacks: `receive() external payable` (empty data, plain ETH) and `fallback() external payable` (unmatched selector).

### Events
```solidity
event Transfer(address indexed from, address indexed to, uint256 value);
emit Transfer(msg.sender, to, value);
```
Stored in **tx receipts**, not storage. Up to 4 `indexed` topics.

---

## 7. Security pitfalls

### Reentrancy bug (Bank example)
```solidity
function withdrawBalance() public {
    uint amt = userBalances[msg.sender];
    (bool ok,) = msg.sender.call{value: amt}("");  // ⚠️ external call BEFORE state update
    require(ok);
    userBalances[msg.sender] = 0;
}
```
Attacker's `receive()` re-enters `withdrawBalance` → drains contract.

**Fix 1 — Checks-Effects-Interactions** (most common):
```solidity
require(amt > 0);                // checks
userBalances[msg.sender] = 0;    // effects
(bool ok,) = msg.sender.call{value: amt}(""); // interactions
require(ok);
```

**Fix 2 — Reentrancy guard** (OpenZeppelin `nonReentrant`):
```solidity
bool transient locked;
modifier nonReentrant { require(!locked); locked = true; _; locked = false; }
```

### Other gotchas
- Contracts **cannot keep secrets** — anyone reads storage via `eth_getStorageAt`.
- `block.timestamp` is miner-influenceable to a small extent — never use as RNG seed alone.
- Integer over/underflow checked since Solidity 0.8 — but watch out for `unchecked { ... }` blocks.
- Use `safeTransferFrom` for tokens that don't follow ERC-20 return convention.
- Front-running: register-then-reveal commit/reveal schemes prevent it.

---

## 8. ERC-20 (fungible tokens)
```solidity
function transfer(address to, uint256 v)            external returns(bool);
function transferFrom(address from, address to, uint256 v) external returns(bool);
function approve(address spender, uint256 v)        external returns(bool);
function totalSupply()                              external view returns(uint256);
function balanceOf(address owner)                   external view returns(uint256);
function allowance(address owner, address spender)  external view returns(uint256);
event Transfer(address indexed from, address indexed to, uint256 value);
event Approval(address indexed owner, address indexed spender, uint256 value);
```
Mental model: each ERC-20 contract maintains its own `mapping(address => uint256) balances`.

## ERC-721 (NFTs)
```solidity
function balanceOf(address owner) external view returns(uint256);
function ownerOf(uint256 tokenId) external view returns(address);
function transferFrom(address from, address to, uint256 tokenId) external payable;
function approve(address approved, uint256 tokenId) external payable;
event Transfer(address indexed from, address indexed to, uint256 indexed tokenId);
event Approval(address indexed owner, address indexed approved, uint256 indexed tokenId);
```

---

## 9. DeFi

### Stablecoins

| | centralized | algorithmic |
|---|---|---|
| **collateralized** | USDC, USDT (custodial) | DAI, RAI (synthetic, over-collateralized) |
| **un(der)collateralized** | CBDC | (historically failed: UST) |

**Custodial flow**: user → 100 USD → bank → `mint(user, 100)` on-chain. Transfers happen on-chain (custodian uninvolved). Withdraw → custodian burns tokens, returns USD.

### Lending (Aave, Compound)
- **Over-collateralized loans**: `LTV × value(collateral) > value(debt)` to borrow. e.g. LTV(ETH)=80.5%.
- **Liquidation threshold** > LTV (e.g. 83% for ETH). If `LiqTh × value(collateral) < value(debt)`, anyone may call `liquidate(borrower, collateralAsset, borrowAsset, amount)` and seize collateral at a discount (the penalty).

```
BorrowCapacity = Σ value(collateral_i) × LTV_i
health         = (Σ value(collateral_i) × LiqThreshold_i) / value(totalDebt)
health < 1   →   liquidation triggered
```

- **cTokens** (Compound): supplier deposits asset, mints cTokens (ERC-20). cToken/asset exchange rate increases over time as interest accrues.
- **Interest rate** = `BaseRate + utilization × slope`, where `utilization = totalBorrow / (available + totalBorrow)`.

### Flash loans
**Borrow + repay in the SAME transaction.** Tx reverts if not repaid → zero risk for lender, no collateral for borrower.

Aave pattern:
```solidity
function flashLoan(address _receiver, uint256 _amount) {
    core.transferToUser(_reserve, userPayable, _amount);
    receiver.executeOperation(_reserve, _amount, fee, _params);
    require(availableLiquidityAfter == availableLiquidityBefore + fee, "balance inconsistent");
}
```
Uses: risk-free arbitrage between DeXes, collateral swap, oracle-manipulation attacks.

### DeX (decentralized exchange)
Non-custodial, permissionless, transparent. Spot vs perpetual. Two main pricing models: order book (rare on-chain) and AMM with constant-product formula `x · y = k` (Uniswap).

---

## 10. CryptoZombies patterns (from your repo)

### Inheritance chain
```
ZombieFactory ← ZombieFeeding ← ZombieHelper ← ZombieSkills ← ZombieAttack ← ZombieOwnership
ZombieMarketplace (standalone, takes ZombieOwnership address)
```

### Patterns you should recognize

**Cooldown via `readyTime`**
```solidity
zombie.readyTime = uint32(block.timestamp + cooldownTime);
function _isReady(Zombie storage z) internal view returns (bool) {
    return z.readyTime <= block.timestamp;
}
```

**Access control modifier**
```solidity
modifier onlyOwnerOf(uint256 _id) {
    require(msg.sender == zombieToOwner[_id], "not owner");
    _;
}
```

**Owner-funded action (level up)**
```solidity
function levelUp(uint256 _id) external payable {
    require(msg.value == levelUpFee);
    zombies[_id].level++;
}
```

**Withdraw accumulated ETH**
```solidity
function withdraw() external onlyOwner {
    (bool ok, ) = payable(owner()).call{value: address(this).balance}("");
    require(ok, "Transfer failed");
}
```

**Skill slot encoding** (avoid "0 means unset" collision with index 0):
```solidity
mapping(uint256 => uint256) equippedSkillSlot;  // skillId + 1; 0 = none
```

**ERC-721-style transfer with approvals**
```solidity
function transferFrom(address from, address to, uint256 tokenId) external payable {
    require(zombieToOwner[tokenId] == msg.sender || zombieApprovals[tokenId] == msg.sender);
    _transfer(from, to, tokenId);
}
```

**Marketplace listing with re-entrancy guard & fees**
```solidity
function buyZombie(uint256 _id) external payable nonReentrant {
    Listing storage l = listings[_id];
    require(l.active && msg.value == l.price);
    l.active = false;                                // effects FIRST
    uint256 fee = (l.price * feeBps) / 10000;
    accumulatedFees += fee;
    zombieContract.transferFrom(l.seller, msg.sender, _id);   // interactions
    (bool ok, ) = payable(l.seller).call{value: l.price - fee}("");
    require(ok);
}
```

### Designing a new feature (exam template)
1. **State**: new struct + mapping(s) (e.g. `mapping(uint256 => SaleInfo) public listings;`).
2. **Events** for all state-changing actions (creation, update, deletion).
3. **Modifiers** for access control (`onlyOwnerOf`, `onlyOwner`).
4. **CEI pattern** in every payable function (checks → effects → interactions).
5. **Use `nonReentrant`** for any function that sends ETH.
6. **Owner-settable params** with bounds (e.g. `require(fee ≤ MAX_FEE)`).
7. **Constants** for caps (`uint16 public constant MAX_FEE_BPS = 1000;`).
8. **Getters**: return tuples or arrays for off-chain readability.
9. **Use `calldata`** for read-only function args to save gas.
10. **Emit indexed events** so off-chain code can filter.

---

## 11. Quick formulas & numbers

```
1 ETH       = 10^18 Wei
1 Gwei      = 10^9  Wei
gasPrice    = min(maxFee, baseFee + maxPriorityFee)
basefee adj = ±12.5 % per block (target 22.5M, max 45M gas)
slot        = 12 s, epoch = 32 slots ≈ 6.4 min
validator   = 32 ETH stake
ERC-20 sig  = bytes4(keccak256("transfer(address,uint256)")) = 0xa9059cbb
SSTORE      = 20k zero→nz, 5k nz→nz, 15k refund on nz→zero
CREATE      = 32000 + 200 × code_size
nonce       = (#Tx sent) + (#contracts created)  — anti-replay
```

---

## 12. Common exam traps

- **`view` vs `pure`**: `view` reads storage; `pure` doesn't touch it.
- **`msg.value` vs `address(this).balance`**: value sent in this call vs total contract ETH.
- **`payable` is required** to receive ETH — function and recipient address both.
- **External calls** (low-level `.call`) **don't revert** on failure — check return bool.
- **`transfer` (2300 gas)** is no longer recommended because of changing opcode costs — prefer `call{value:}("")` with CEI + `nonReentrant`.
- **Storage layout**: variables `< 32 B` packed into 32-byte slots if declared next to each other (gas saving).
- **Mapping default value** is zero — always check / use sentinel encoding for "unset".
- **`block.timestamp` is NOT random**. Use `keccak256(abi.encodePacked(block.timestamp, msg.sender, nonce))` for weak randomness; for strong RNG use Chainlink VRF.
- **Don't store secrets in contracts.** Storage is publicly readable.
- **Underflow / division by zero** revert (since 0.8). Don't rely on saturating math.
