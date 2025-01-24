
# Theory

## Understanding B-tree order and properties

A **B-tree of order* $m$ has the following properties:
**1. Internal (non-leaf) nodes:**
    - Can have a maximum of $m$ children and $m - 1$ keys.
    - Can have a minimum of $\lceil m/2 \rceil$ children.
**2. Leaf nodes:**
    - Must all appear at the same level (balanced property).

---

## Union rule in functional dependencies

**Functional dependency rule (Union Rule):**

$\text{If } \alpha \to \beta \text{ and } \alpha \to \gamma, \text{ then } \alpha \to \beta \gamma$

This means if $\alpha$ determines both $\beta$ and $\gamma$ separately, it must also determine their combination, $\alpha \to \beta \gamma$.

---

![alt text](joinscheatsheet.png)

---
