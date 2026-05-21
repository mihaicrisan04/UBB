# Lecture 10 — Decentralized Exchanges (DeX)

## Recap — lending protocols
- Over-collateralized loans: `LTV × value(collateral) > value(debt)`
- Liquidation when `LiqThreshold × value(collateral) < value(debt)` (i.e. `health < 1`)
- Liquidity providers send assets → get representative tokens (cTokens) → redeem anytime
- Borrowers pay interest → goes to liquidity providers
- On default or liquidation → borrower loses (part of) collateral

## Flash loans recap
- Loan + repayment in the **same transaction**, otherwise Tx reverts
- Lender = zero risk, borrower = zero collateral
- Use cases: arbitrage, collateral swap, oracle-manipulation attacks

---

## Two types of exchanges

### Spot exchange
Exchange X of asset A for Y of asset B based on the **current** spot price.

### Perpetual exchange
Bet on a **future** exchange rate (e.g. pay X now for soy beans in 3 months) — derivatives.

> This lecture focuses on **spot exchanges**.

---

## What is a spot exchange?
Ethereum has many ERC-20 tokens:
- **WETH** (wrapped ETH as ERC-20), **stETH** (staked ETH)
- USDC, USDT, DAI — USD stablecoins
- **Governance tokens** (GTC for Gitcoin)
- **Gaming tokens**

An exchange converts one token into another (e.g. USDC → GTC). Two questions:
1. What's the **exchange rate**?
2. How are **sellers and buyers** connected?

---

## First approach — Centralized Exchange (CeX)

```
[Bob (5 ETH)] → "want to exchange 2 ETH for USDC" → [BANK / CeX]
              ← "rate: 1600 USDC/ETH"             ← Treasury: 1000 ETH, 10000 USDC, 10 GTC
              → send 2 ETH →
              ← send 3200 USDC ←
              
final: Bob has 3 ETH + 1600 USDC,  treasury has 1002 ETH + 6800 USDC
```

### Order types
**Limit order**: "I'll buy 1 ETH for up to 1700 USDC for the next 24 hours". CeX fills it (or not). The list of buy/sell orders = **order book**.

### Issues
- **Rate determination**: by supply/demand at the exchange — **not transparent**; competing exchanges → bad UX
- **Security**: what if the exchange takes Bob's 2 ETH but never sends USDC?
- **Censorship**: what if the exchange refuses to do business with Bob?

---

## A more trusted solution — DeX

### What is a DEX?
A **marketplace where transactions happen directly between participants, without a trusted intermediary**.

### Properties
- **Programmable**: usable as a service by other contracts (composability)
- **Transparent**: code is on-chain and inspectable
- **Permissionless**: anyone can use
- **Non-custodial**: never holds user assets

### DEX market share (2019 → 2025)
Share of decentralized exchange spot trading volume rose from <1 % in 2019 to ~ 20 % in 2025. DEXes have eaten significant CeX share.

---

## Implementation models (background)

### Order-book DEX (rare on-chain)
Like a CEX but with on-chain matching. **Expensive** (gas per order/cancel/match), so mostly used off-chain or hybrid (e.g. dYdX v3).

### Automated Market Maker (AMM)
The dominant on-chain model. Pools of two assets `(x, y)` with an invariant — e.g. Uniswap's **constant product**:
```
x · y = k
```
Anyone can swap by adjusting the pool, paying a small fee. The price emerges from `x/y` ratio.

> Lecture 10 stopped here — AMM mechanics covered in detail next week (likely subsequent material).

---

## Exam-relevant takeaways
- A DEX is **non-custodial** — assets stay with the user / in the smart contract, not with a third party
- The four DEX properties: **programmable, transparent, permissionless, non-custodial**
- Centralized exchanges have **opaque price discovery, security risk (rug pulls), and censorship power**
- AMMs (e.g. Uniswap) use a `x · y = k` invariant to make markets without an order book
- Flash loans + AMMs together enable **oracle-manipulation attacks** when contracts use spot pool prices as a price oracle
