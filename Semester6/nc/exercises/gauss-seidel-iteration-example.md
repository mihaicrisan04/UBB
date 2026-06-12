# Gauss–Seidel Method — Worked Example (3 iterations)

**Problem.** Compute the first three iterations of the Gauss–Seidel method for the system, starting from $x^{(0)} = [0, 0]^T$:

$$\begin{cases} 2x_1 + x_2 = -1 \\ x_1 + 2x_2 = -2 \end{cases}$$

## Method (Lecture 3 component form)

Gauss–Seidel solves each equation for its diagonal unknown, **reusing freshly computed components within the same sweep** (unlike Jacobi, which uses only the previous iterate):

$$x_i^{(k+1)} = \frac{1}{a_{ii}}\left[b_i - \sum_{j<i} a_{ij}\,x_j^{(k+1)} - \sum_{j>i} a_{ij}\,x_j^{(k)}\right].$$

For this system:

$$x_1^{(k+1)} = \frac{1}{2}\big(-1 - x_2^{(k)}\big), \qquad x_2^{(k+1)} = \frac{1}{2}\big(-2 - x_1^{(k+1)}\big).$$

Note the $x_2$ update uses the **just-computed** $x_1^{(k+1)}$.

**Convergence is guaranteed:** the matrix is strictly diagonally dominant ($|2| > |1|$ in both rows), so Gauss–Seidel converges (at least as fast as Jacobi).

## Iterations (start $x^{(0)} = [0, 0]^T$)

**Iteration 1:**
$$x_1^{(1)} = \tfrac12(-1 - 0) = -\tfrac12, \qquad x_2^{(1)} = \tfrac12\big(-2 - (-\tfrac12)\big) = \tfrac12\left(-\tfrac32\right) = -\tfrac34.$$
$$x^{(1)} = \left[-\tfrac12,\ -\tfrac34\right]^T$$

**Iteration 2:**
$$x_1^{(2)} = \tfrac12\big(-1 - (-\tfrac34)\big) = \tfrac12\left(-\tfrac14\right) = -\tfrac18, \qquad x_2^{(2)} = \tfrac12\big(-2 - (-\tfrac18)\big) = \tfrac12\left(-\tfrac{15}{8}\right) = -\tfrac{15}{16}.$$
$$x^{(2)} = \left[-\tfrac18,\ -\tfrac{15}{16}\right]^T$$

**Iteration 3:**
$$x_1^{(3)} = \tfrac12\big(-1 - (-\tfrac{15}{16})\big) = \tfrac12\left(-\tfrac1{16}\right) = -\tfrac1{32}, \qquad x_2^{(3)} = \tfrac12\big(-2 - (-\tfrac1{32})\big) = \tfrac12\left(-\tfrac{63}{32}\right) = -\tfrac{63}{64}.$$

$$\boxed{\ x^{(3)} = \left[-\tfrac1{32},\ -\tfrac{63}{64}\right]^T = [-0.03125,\ -0.984375]^T\ }$$

## Note: convergence vs Jacobi

The exact solution is $x = [0, -1]^T$. Comparing iteration 3:

| method | $x^{(3)}$ |
|---|---|
| Jacobi | $[-0.125,\ -1]$ |
| Gauss–Seidel | $[-0.03125,\ -0.984375]$ |

Gauss–Seidel is noticeably closer — reusing updated components within each sweep speeds up convergence.

## Key takeaway

- Gauss–Seidel: as you sweep $i = 1, 2, \dots, n$, immediately use any $x_j^{(k+1)}$ already computed in this sweep ($j < i$); use the old $x_j^{(k)}$ only for the not-yet-updated ones ($j > i$).
- Per step: isolate $x_i$ from equation $i$, divide by $a_{ii}$.
- Strict diagonal dominance guarantees convergence.
- Matrix form: $x^{(k+1)} = T_{GS}\,x^{(k)} + c_{GS}$ with $T_{GS} = (D-L)^{-1}U$, $c_{GS} = (D-L)^{-1}b$.
