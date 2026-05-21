# Lecture 8 — Solidity

## Recap
- World state = set of accounts (EOA or contract)
- Each contract owns a private storage array `S[bytes32] → bytes32`
- Tx: `to, from, value (wei), gasLimit/maxFee/maxPriorityFee, calldata`, signed by EOA
- Contracts cannot keep secrets — anyone can read storage via `eth_getStorageAt`

> Solidity code → `solc` compiler → EVM bytecode → executed by validators.

---

## Contract skeleton
```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import "@openzeppelin/contracts/access/Ownable.sol";

interface IERC20 {
    function transfer(address _to, uint256 _value) external returns (bool);
    function totalSupply() external view returns (uint256);
}

contract ERC20 is IERC20 {           // inheritance
    address owner;
    constructor() { owner = msg.sender; }
    function transfer(address _to, uint256 _value) external returns (bool) {
        // implementation
    }
}
```

## Value types
- `uint256`, `intN`, `bool`, `bytes32`, `address` (20 bytes)

**`address` operations:**
```solidity
addr.balance                  // uint256
addr.send(value)              // bool
addr.transfer(value)          // throws on fail (deprecated — fixed 2300 gas)
(bool ok,) = addr.call{value: msg.value/2, gas: 1000}(args);  // generic call
addr.delegatecall(args)       // run another contract's code in MY context
```

## Reference types
- `struct`, arrays `T[]`, `bytes`, `string`, `mapping(K => V)`

```solidity
struct Person { uint128 age; uint128 balance; address addr; }
Person[10] public people;

mapping(address => uint256) balances;   // declaration
balances[addr] = value;                 // assignment
```

> Strings can't be compared directly: compare via `keccak256(bytes(a)) == keccak256(bytes(b))`.

---

## Globally available variables
```solidity
block.number, block.timestamp, block.coinbase, block.gaslimit, block.blockhash
gasleft()
msg.sender, msg.value, msg.data, msg.sig
tx.gasprice, tx.origin
abi.encode / encodePacked / encodeWithSelector / encodeWithSignature
keccak256(bytes), sha256(bytes)
require(cond, "msg"), assert(cond), revert("msg")
```

### `msg.sender` vs `tx.origin`
Call chain `A → B → C → D`: inside D, `msg.sender == C`, `tx.origin == A`.
> Never authenticate users with `tx.origin` — a malicious intermediate contract can impersonate them.

---

## Function visibilities
| keyword | callable from | calldata? |
|---|---|---|
| `external` | only outside | yes (cheapest for big inputs) |
| `public` | outside + inside | args copied to memory |
| `private` | only this contract | — |
| `internal` | this + derived contracts | — |

## State mutability
- `view`: reads storage, never writes
- `pure`: doesn't touch storage at all
- `payable`: can receive ETH (`msg.value > 0` allowed)

```solidity
function f(uint a) private pure returns (uint b) { return a + 1; }
```

---

## Inheritance & libraries

```solidity
contract owned {
    address owner;
    constructor() { owner = msg.sender; }
    modifier onlyOwner { require(msg.sender == owner); _; }
}

contract Destructible is owned {
    function destroy() public onlyOwner { selfdestruct(owner); }
}
```

**Library**: code executes in the **caller's context** (no new frame, no storage).
```solidity
library Search { function indexOf(uint[] storage arr, uint v) returns (uint) {...} }
contract A { uint[] xs; function find(uint v) { Search.indexOf(xs, v); } }
```

---

## ERC-20 (fungible tokens)
A standard interface for fungible tokens — itself a smart contract maintaining `mapping(address => uint256) balances`.

### Interface
```solidity
function transfer(address _to, uint256 _value)            external returns (bool);
function transferFrom(address _from, address _to, uint256 _value) external returns (bool);
function approve(address _spender, uint256 _value)        external returns (bool);
function totalSupply()                                    external view returns (uint256);
function balanceOf(address _owner)                        external view returns (uint256);
function allowance(address _owner, address _spender)      external view returns (uint256);
```

### `transfer` implementation
```solidity
function transfer(address _to, uint256 _value) external returns (bool) {
    require(balances[msg.sender] >= _value, "ERC20_INSUFFICIENT_BALANCE");
    balances[msg.sender] -= _value;
    balances[_to]        += _value;
    emit Transfer(msg.sender, _to, _value);    // log event
    return true;
}
```

---

## Calling other contracts
```solidity
address _token = 0x2b34...;
ERC20Token tokenContract = ERC20Token(_token);  // type cast
(bool ok,) = tokenContract.transfer(_to, _value);
// Compiler handles ABI encoding, calldata copy, return decoding.
```

## ABI encoding & function selector

Calldata = **first 4 bytes (function selector)** + ABI-encoded arguments.

```solidity
selector = bytes4(keccak256("transfer(address,uint256)"));  // 0xa9059cbb
```

### Manual encoding
```solidity
bytes memory action = abi.encodeWithSelector(
    tokenContract.transfer.selector, _to, 1 ether
);
(bool ok,) = address(tokenContract).call(action);
```

### Fallback functions
```solidity
receive() external payable { ... }    // called when calldata is empty (pure ETH transfer)
fallback() external payable { ... }   // called when selector doesn't match any function
```

---

## Data location of variables (gas matters!)

### Stack
- Cheapest, used for `<= 32 byte` simple types
- Only 16 stack vars per scope

### Calldata
- Read-only byte array, the Tx's input data
- **16 gas / non-zero byte**, 4 gas / zero byte
- Cheaper than memory — mark args `calldata` instead of `memory` when possible

### Memory (MSTORE / MLOAD)
- Byte array, mutable, lives for one execution frame
- Complex types (`> 32 B`: structs, arrays, strings) MUST be in memory or storage
- Cheap, but cost grows **quadratically**

### Storage (SSTORE / SLOAD)
- Persistent on-chain
- Very expensive (20k zero→nz, 5k nz→nz)
- Mappings & state vars ALWAYS in storage
- Gas refund when zeroed
- **Packing trick**: vars `< 32 B` declared consecutively get packed into one slot

### Event logs
- Stored in **Tx receipts**, not in storage
- Cheap, only readable by off-chain block explorers (not by other contracts)
```solidity
event Transfer(address indexed from, address indexed to, uint256 value);
emit Transfer(msg.sender, _to, _value);
```

---

## Security considerations
- Check overflow/underflow? **Done by compiler since Solidity 0.8.** Use `unchecked { }` to opt out for gas savings.
- What assertions about inputs, return values, contract state?
- Who is allowed to call each function?
- Any assumptions about external contracts being called?

---

## Re-entrancy bug

### Vulnerable code
```solidity
function withdrawBalance() public {
    uint256 amount = userBalances[msg.sender];
    (bool ok,) = msg.sender.call{value: amount}("");   // ⚠️ external call BEFORE state update
    require(ok);
    userBalances[msg.sender] = 0;                      // ← too late
}
```

### Attacker
```solidity
contract Attacker {
    receive() external payable {
        if (gasleft() > 10000 && address(bank).balance >= 75)
            bank.withdrawBalance();   // re-enter before balance is zeroed
    }
}
```

### Attack flow
```
Attacker.triggerAttack
  → Bank.withdrawBalance
      → Attacker.receive
          → Bank.withdrawBalance
              → Attacker.receive
                  → … (drains contract)
```

### Fix 1 — Checks-Effects-Interactions
```solidity
function withdrawBalance() public {
    uint amt = userBalances[msg.sender];
    require(amt > 0);                                   // checks
    userBalances[msg.sender] = 0;                       // effects FIRST
    (bool ok,) = msg.sender.call{value: amt}("");       // interactions LAST
    require(ok);
}
```

### Fix 2 — Re-entrancy guard
```solidity
bool transient locked;                                   // 'transient' = TSTORE/TLOAD slot
modifier nonReentrant {
    require(!locked, "Reentrancy attempt");
    locked = true;
    _;
    locked = false;
}
function withdrawBalance() public nonReentrant { ... }
```

> OpenZeppelin's `ReentrancyGuard` provides this out of the box.

---

## Exam-relevant takeaways
- **Visibility**: `external` (calldata, cheapest), `public`, `internal`, `private`
- **State mutability**: `view` (reads), `pure` (no storage), `payable` (receives ETH)
- **Data location** matters for gas: `calldata < memory ≪ storage`
- Use `calldata` for read-only function args on `external` functions
- `tx.origin` is the original EOA, `msg.sender` is the direct caller — **never authenticate with tx.origin**
- **Re-entrancy**: external call before state update lets attacker recurse. Fix: CEI or `nonReentrant`
- ERC-20 = standard fungible-token interface, ERC-721 = NFT
- Function selector = first 4 bytes of `keccak256("name(types)")`
- Solidity 0.8+ has built-in overflow/underflow checks
