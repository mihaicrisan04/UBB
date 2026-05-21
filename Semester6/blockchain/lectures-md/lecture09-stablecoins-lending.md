# Lecture 9 — Stablecoins & Lending Protocols

## The world of DeFi
DeFi = on-chain financial instruments built from composable smart contracts.

```
[user] → [Aave (lending)] → [ERC-20 coin B] → "transfer 100 from me to Alice"
[user] → [Uniswap (DEX)]   → exchange coin A ↔ coin B
[user] → [Morpho (lending)]
```
Building blocks: ERC-20 (fungible), ERC-721 (NFTs), various protocols (Aave, Uniswap, Morpho).

---

# Part 1 — Stablecoins

## Goal
A cryptocurrency designed to trade at a fixed price:
- 1 coin = 1 USD (USDC, USDT)
- 1 coin = 1 EUR
- 1 coin = 1 USDX (some synthetic asset)

**Why?** Integrate real-world currencies into on-chain apps; let people in inflationary economies hold USD-equivalent assets.

## Types
|  | **centralized** | **algorithmic** |
|---|---|---|
| **collateralized** | custodial stablecoins (USDC, USDT) | synthetics (DAI, RAI) |
| **un(der)collateralized** | central bank digital currency | (historical failures: UST/Luna) |

---

## Custodial stablecoins (USDC, USDT)

### Minting
```
[user] → "100 USD"   → [BANK custodian] → mint(user, 100)  → on-chain state {Bob: 100}
                       holds 100 USD in treasury
```

### Transfer
```
[user] → "transfer(Bob → Carol, 15)" → on-chain state: {Bob: 85, Carol: 15}
```
> **Transfers are on-chain only — the custodian is NOT involved**. They're just ERC-20 transfers.

### Withdrawal
```
[user] → "withdraw 60 USD" → [BANK] → burn(Bob, 60)
                            ← 60 USD to user
                            treasury: 135 → 75 USD
```

### Stats (Sep 2025)
| | Coins issued | 24 h volume |
|---|---|---|
| USDC | 76 B | 20.4 B |
| USDT | 184 B | 164.6 B |

Stablecoin providers are among the **largest holders of US Treasury debt** (alongside major sovereign holders).

### Issues
- Custodian must be **audited** to prove treasury exists
- Custodian earns interest on the USD deposits — profit center
- Custodian can **freeze accounts**, refuse withdrawals, **remove funds** from balances (centralized power)

### Why USDC in Argentina?
Inflation example: ARS purchasing power dropped 82 % since Nov 2022; stablecoin trading on local exchanges grew > 10 000 %.

---

# Part 2 — Lending protocols

## Banks 101
Banks bring together **lenders** (depositors) and **borrowers**. Spread = `borrowInterest − depositInterest` = bank's profit. Bank assumes the risk of borrower default (Alice gets her deposit back either way).

## CeFi lending (Nexo, BlockFi)
Same as a bank, but for crypto:
```
[Alice] → deposit ETH → [CeFi institution] → borrow ETH → [Bob]
       ← deposit interest                  ← borrow interest
```

### The role of collateral
CeFi's concern: what if Bob defaults? Solution = require Bob to **lock up collateral**.

```
[CeFi] ← deposit 500 UNI ← [Bob]    (1 ETH = 100 UNI)
       → borrow 1 ETH    →
                                    debt position: +500 UNI, −1 ETH
```
This is an **over-collateralized loan**: value(collateral) > value(debt).

### Three scenarios

**(1) Bob repays the loan**
```
[CeFi] ← 1 ETH ← [Bob]
       → 500 UNI − interest →
```

**(2) Bob defaults**
```
[CeFi] ← "can't repay 1 ETH" ← [Bob]
        keeps (100 + penalty) UNI worth 1 ETH
        Bob redeems remaining (400 − interest − penalty) UNI
```

**(3) Liquidation** — collateral value drops
```
Suppose price moves to 1 ETH = 400 UNI
debt = 1 ETH = 400 UNI, collateral = 500 UNI
CeFi liquidates *before* value(debt) > value(collateral):
  → sells 100 UNI to clear debt, charges penalty of 20 UNI
  → returns 380 UNI to Bob
```
> Lender MUST liquidate **before** `value(debt) > value(collateral)`.

---

## Terminology

### Loan-to-Value (LTV) ratio — borrow capacity
`LTV × value(collateral) > value(debt)` is the constraint to **borrow more**.

- High-volatility asset → low LTV
- Stable asset → higher LTV

**Examples (Aave)**:
| asset | LTV | Liquidation threshold |
|---|---|---|
| ETH | 80.5 % | 83 % |
| USDC | 75 % | 78 % |
| DAI | 63 % | 77 % |

### Health factor
```
BorrowCapacity = Σ value(collateral_i) × LTV_i
health         = (Σ value(collateral_i) × LiqThreshold_i) / value(totalDebt)
```
- `health ≥ 1`: position is healthy
- `health < 1`: triggers **liquidation** until health ≥ 1 again

### Why borrow ETH at all?
- May need ETH (for in-game assets, etc.) **without selling** existing collateral (e.g. an NFT)
- Investment strategy: borrow against existing stake to get exposure to additional asset (leverage)

## Problem with CeFi
- Users must **trust** the CeFi institution (not to get hacked, steal, miscalculate)
- Interest goes to the exchange, not the liquidity provider
- CeFi fully controls the spread

→ Can we do this **on-chain** with no central trusted party? Code visible for inspection.

---

## DeFi Lending

### First idea — order book
Lenders post supply, borrowers post collateral, "price match" engine matches them.

**Challenges**:
- Computationally expensive (many txs per match)
- **Concentrated risk** — each lender directly exposed to one borrower's default
- Complex withdrawal — lender waits for the counter-party to repay

### Better approach — liquidity pools (Compound, Aave)
All liquidity providers pool their assets per market. Borrowers borrow from the pool.

```
[Alice]  → supply assets → [DAI pool, ETH pool, UNI pool, AXS pool] ← supply assets ← [Bob]
[Carol] → supply assets →                                          ← borrow ETH    ← [Dave]
liquidity providers (earn interest)                                  borrowers
```

### Compound cTokens
- Liquidity provider deposits an asset (e.g. 10 ETH) → mints **cETH** (an ERC-20 representing share of the pool)
- Exchange rate `asset / cToken` increases over time as interest accrues
- Provider redeems cTokens at any time to get back asset + interest

### Borrowing flow
1. Bob supplies asset to any market → receives cTokens
2. Bob calls `borrow(ETH, amount)` → his cTokens **locked as collateral**
3. Compound sends ETH to Bob
4. Bob's accrued interest raises the ETH/cETH exchange rate → benefits **cETH holders**

### Interest rate model
**Utilization ratio**:
```
U_ETH = totalBorrowBalance_ETH / (availableBalance_ETH + totalBorrowBalance_ETH) ∈ [0, 1]
```
High utilization → asset is in demand → rate goes up.

```
interestRate_ETH = BaseRate_ETH + U_ETH × slope_ETH
```

### Liquidation — `health < 1`
**Anyone** can call:
```solidity
liquidate(borrower, CollateralAsset, BorrowAsset, uint amount)
```
- The liquidator **repays part of the borrower's debt** in `BorrowAsset` (e.g. ETH)
- Receives `CollateralAsset` (e.g. cDAI) **at a discounted exchange rate** — the penalty for the borrower, profit for the liquidator

### Why liquidations happen
- Collateral price drops → debt/collateral ratio worsens → health < 1
- Debt APR spikes (e.g. UNI demand spikes → UNI price spikes → debt value spikes)
- Borrowers must constantly monitor health (automated services help)

### Stats (Oct 2025)
- Aave Daily TVL: ~ $60 B
- Aave outstanding debt: ~ $30 B

---

## Flash loans

### Definition
A loan **taken and repaid in the same transaction**. If not repaid, the Tx reverts.

- ⇒ **Zero risk for lender**
- ⇒ **Borrower needs NO collateral**

### Aave v1 implementation
```solidity
function flashLoan(address _receiver, uint256 _amount) {
    core.transferToUser(_reserve, _receiver, _amount);
    receiver.executeOperation(_reserve, _amount, fee, _params);  // borrower does stuff
    require(availableLiquidityAfter == availableLiquidityBefore + fee,
            "balance inconsistent");                              // abort if not repaid
}
```

### Use cases

**Risk-free arbitrage**: Alice finds a price difference between two pools
```
   1 USDC = 1.002 DAI on Uniswap
   1 USDC = 1.001 DAI on Curve

1. flashLoan 1M USDC from Aave
2. swap 1M USDC → 1.002M DAI on Uniswap
3. swap 1.002M DAI → 1.001M USDC on Curve
4. repay 1M USDC + fee to Aave
5. keep ~0.001M USDC profit
```
All in a single transaction.

**Collateral swap**: Alice has a 1000-DAI debt collateralized by 1 cETH at Compound. She wants to swap collateral to cUSDC without unwinding the position.
```
1. flashLoan 1000 DAI from Aave
2. repay 1000 DAI debt at Compound
3. redeem 1 cETH from Compound
4. swap 1 cETH → 1500 cUSDC on a DEX
5. deposit 1500 cUSDC as collateral
6. borrow 1000 DAI at Compound
7. repay 1000 DAI flash loan
```

**DeFi attacks**: price-oracle manipulation. If a contract reads asset prices from a DEX pool, a flash loan can temporarily distort that pool and exploit the contract.

### Aave flash loan volume
~ $21.6M in 24 h on Oct 2025 (208 flash loans, 9 unique borrowers).

---

## Exam-relevant takeaways
- Stablecoins peg to USD via collateral (centralized USDC/USDT) or algorithm (DAI/RAI)
- Custodial stablecoin transfers happen **on-chain only** — the custodian only matters for mint/burn
- Lending requires **over-collateralization**: `LTV × value(collateral) > value(debt)` to borrow more
- **Liquidation triggered when `health < 1`** — anyone can liquidate, paid with a discount on the collateral
- Compound uses **cTokens** — depositors earn interest via a rising exchange rate
- **Flash loans**: borrow + repay in one Tx → no collateral needed → used for arbitrage, collateral swaps, and attacks
