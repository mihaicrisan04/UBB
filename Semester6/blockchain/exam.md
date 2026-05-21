
Part I - Theory Questions
Select a single correct answer for each question.

1. what does the emit keyword do in solidity?
a. sned eth to an address
b. publishes an event to the blockchain
c. creates a new contract
d. deletes a variable from storage

2. what is the diffrecen betwen storage and memory in solidity? 
a. stoare is temporary, memory is persistent
b. storage is persisten on the blockchain, memory is temporary during excecution
c. stoare is faster than memory
d. there is no diffrence

3. What happens when you access a non-existent key in a mapping ?
A. An error is thrown
B. null is returned
C. The default value of the type is returned (e.g. false, 0, address (0))
D. The contract stops

4. What does msg. value represent in a Solidity function?
A. The address of the called contract
B. The amount of ETH sent along with the current transaction
C. The current block number
D. The gas limit of the transaction

5. When we write contract B is A, what does B receive?
A. An independent copy of A, with no connection
B. Access to the non-private functions and variables of A
C. A is deleted and replaced by B
D. B can only call external functions of A


Part II - Practical Exercise

Contract Description
The contract below implements a basic decentralized crowdfunding campaign. It tracks a funding goal and a running total of ETH raised,
and allows anyone to contribute any positive amount. There is no contributor tracking, no refund mechanism, and no access control over
the collected funds.
Your task is to extend this contract by tracking contributors, enforcing contribution rules, implementing a refund mechanism, introducing
ownership through inheritance, and adding goal-based fund withdrawal.
Starting Contract

```sol
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;
contract Crowdfunding f
address public owner;
uint public goal;
uint public totalRaised;
constructor(uant_goal) {
owner = msg. sender;
goal = _goal;
}
function contribute() public payable {
require(msg. value > 0, "Must send ETH");
totalRaised += msg. value;
}
}
```
6. Contributor struct + storage 
Define a Contributor struct containing: - amount - hasContributed
Define a data structure to associate an address with a contributor.

7. Contribution function 
Modify the contribute function so that: - the same address cannot contribute more than once - the contributor's data must be saved in
the contract

8. Refund function 
Create a refund function that allows a contributor to recover their funds.
Conditions: - can only be called if the goal has not been reached - only addresses that have contributed can call the function - the
contributed amount is transferred back to the caller's address - after the refund, the contributor is no longer marked as having an active
contribution

9. Helper contract and inheritance
Create a separate OwnableHelper contract containing: - the owner variable - owner initialization - an onlyowner modifier
Modify the Crowdfunding contract to inherit from OwnableHelper.

10. Fund withdrawal and read-only function
Create the withdraw function that: - can only be called by the owner - can only be called if the goal has been reached - transfers all
collected funds to the owner
Create a read-only function getCampaignInfo that returns: the amount raised, the goal, and whether the goal has been reached.
