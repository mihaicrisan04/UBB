# Lecture Notes — Blockchain Smart Contracts

Concise markdown summaries of each lecture, focused on what's likely to appear on the exam.

| # | Title | Topics |
|---|---|---|
| [01](lecture01-intro.md) | Introduction & Cryptography Background | what is a blockchain · layers · safety/liveness · hash functions · Merkle trees · PoW · signatures |
| [03](lecture03-bitcoin-scripts-wallets.md) | Bitcoin Scripts & Wallets | block/tx structure · P2PKH · P2SH · multisig · SegWit · HD wallets (BIP-39) · SPV |
| [04](lecture04-consensus-fundamentals.md) | Fundamentals of Consensus | Byzantine Generals · BB · Dolev-Strong (f+1 rounds) · SMR · safety vs liveness |
| [05](lecture05-nakamoto-consensus.md) | Nakamoto Consensus (Bitcoin) | longest-chain · k-deep confirmation · private attack · β<1/2 · MEV |
| [06](lecture06-pos-incentives.md) | Incentives & Proof-of-Stake | block rewards · MEV · slashing · PBFT-style PoS · 2n/3 quorum · accountable safety |
| [07](lecture07-ethereum-mechanics.md) | Ethereum Mechanics | EOA vs contracts · EVM · storage/memory/transient · gas & EIP-1559 (burn) |
| [08](lecture08-solidity.md) | Solidity | contract syntax · visibility · data locations · ERC-20 · ABI · re-entrancy + fixes |
| [09](lecture09-stablecoins-lending.md) | Stablecoins & Lending | USDC/USDT · DAI · over-collateralized loans · health factor · cTokens · flash loans |
| [10](lecture10-dex.md) | Decentralized Exchanges | CEX vs DEX · non-custodial properties · AMM (x·y=k) |

## How to use
Read the lecture file matching the exam question's topic. Each file ends with an **"Exam-relevant takeaways"** section that distills the slide deck into the points worth memorizing.

For quick cross-topic lookups, see [`../CHEATSHEET.md`](../CHEATSHEET.md).
