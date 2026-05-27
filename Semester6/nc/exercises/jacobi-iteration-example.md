# Jacobi Method — Worked Example (3 iterations)

**Problem.** Approximate the solution of the system by the **third** Jacobi iteration, starting from $x^{(0)} = [0, 0]^T$:

$$\begin{cases} 2x_1 + x_2 = -1 \\ x_1 + 2x_2 = -2 \end{cases}$$

## Method (Lecture 3 component form)

Jacobi solves each equation for its diagonal unknown, using **only the previous iterate** on the right-hand side:

$$x_i^{(k+1)} = \frac{1}{a_{ii}}\left[b_i - \sum_{j \neq i} a_{ij}\, x_j^{(k)}\right], \qquad i = \overline{1,n}.$$

For this $2 \times 2$ system ($a_{11} = a_{22} = 2$, $a_{12} = a_{21} = 1$, $b = (-1, -2)^T$):

$$x_1^{(k+1)} = \frac{1}{2}\big(-1 - x_2^{(k)}\big), \qquad x_2^{(k+1)} = \frac{1}{2}\big(-2 - x_1^{(k)}\big).$$

**Convergence is guaranteed:** the matrix is strictly diagonally dominant ($|a_{11}| = 2 > |a_{12}| = 1$ and $|a_{22}| = 2 > |a_{21}| = 1$), which gives $\|T_J\|_\infty < 1$.

## Iterations (start $x^{(0)} = [0, 0]^T$)

**Iteration 1:**
$$x_1^{(1)} = \tfrac12(-1 - 0) = -\tfrac12, \qquad x_2^{(1)} = \tfrac12(-2 - 0) = -1.$$
$$x^{(1)} = \left[-\tfrac12,\ -1\right]^T$$

**Iteration 2:**
$$x_1^{(2)} = \tfrac12\big(-1 - (-1)\big) = 0, \qquad x_2^{(2)} = \tfrac12\big(-2 - (-\tfrac12)\big) = \tfrac12\left(-\tfrac32\right) = -\tfrac34.$$
$$x^{(2)} = \left[0,\ -\tfrac34\right]^T$$

**Iteration 3:**
$$x_1^{(3)} = \tfrac12\big(-1 - (-\tfrac34)\big) = \tfrac12\left(-\tfrac14\right) = -\tfrac18, \qquad x_2^{(3)} = \tfrac12(-2 - 0) = -1.$$

$$\boxed{\ x^{(3)} = \left[-\tfrac18,\ -1\right]^T = [-0.125,\ -1]^T\ }$$

## Note: the iterates converge to the exact solution

The exact solution is $x = [0, -1]^T$ (from $3x_1 = 0$ after eliminating $x_2$). The iterates close in:

| $k$ | $x_1^{(k)}$ | $x_2^{(k)}$ |
|----|-----------|-----------|
| 0  | $0$       | $0$       |
| 1  | $-0.5$    | $-1$      |
| 2  | $0$       | $-0.75$   |
| 3  | $-0.125$  | $-1$      |

## Key takeaway

- Jacobi: update *all* components simultaneously from the **previous** iterate $x^{(k)}$ (do **not** reuse freshly computed values within the same sweep — that would be Gauss–Seidel).
- Per step: isolate $x_i$ from equation $i$, divide by $a_{ii}$.
- Strict diagonal dominance $|a_{ii}| > \sum_{j\neq i}|a_{ij}|$ guarantees convergence.
- Matrix form (equivalent): $x^{(k+1)} = T_J x^{(k)} + c_J$ with $T_J = D^{-1}(L+U)$, $c_J = D^{-1}b$.
