# Lecture 2 — Divided & Finite Differences (Properties) and Direct Methods for Linear Systems

## Overview
This lecture completes the study of divided differences (their algebraic properties and the mean-value formula) and introduces finite differences (forward $\Delta$ and backward $\nabla$), linking them to divided differences for equally spaced nodes. It then opens Chapter 2 on solving linear systems $Ax = b$, focusing on direct methods: triangular-system solving, Gaussian elimination with pivoting strategies, computational complexity, and LU factorization.

## Key Notions & Definitions

- **Forward difference** — for equidistant nodes $x_i = x_0 + ih$, $h > 0$, $f_i = f(x_i)$:
$$\Delta^1 f(x_i) = f(x_{i+1}) - f(x_i) = f_{i+1} - f_i,$$
$$\Delta^k f(x_i) = \Delta^{k-1}f(x_{i+1}) - \Delta^{k-1}f(x_i) = \Delta^{k-1}f_{i+1} - \Delta^{k-1}f_i.$$
Convention: $\Delta^0 f(x_i) = f_i$.

- **Backward difference** — $\nabla^0 f_i = f_i$, $\nabla^1 f_i = f_i - f_{i-1}$, $\nabla^k f_i = \nabla^{k-1}f_i - \nabla^{k-1}f_{i-1}$.

- **Finite differences** — collective name for forward and backward differences; "finite" with nothing specified means *forward*.

- **Triangular matrix** — $A = [a_{ij}]_{i,j=\overline{1,n}}$ is *upper triangular* if $a_{ij} = 0$ for all $i > j$; *lower triangular* if $a_{ij} = 0$ for all $i < j$; *diagonal* if both ($a_{ij} = 0$ for all $i \neq j$).

- **Backward substitution** — solving a nonsingular upper triangular $Ux = b$:
$$x_n = \frac{b_n}{u_{nn}}, \qquad x_i = \frac{1}{u_{ii}}\Big(b_i - \sum_{j=i+1}^{n} u_{ij}x_j\Big), \quad i = \overline{n-1, 1}.$$

- **Forward substitution** — solving a nonsingular lower triangular $Lx = b$:
$$x_1 = \frac{b_1}{l_{11}}, \qquad x_i = \frac{1}{l_{ii}}\Big(b_i - \sum_{j=1}^{i-1} l_{ij}x_j\Big), \quad i = \overline{2, n}.$$

- **Gaussian elimination** — transforming a system into an equivalent upper triangular one using elementary row operations: $(\lambda R_i) \to (R_i)$, $(R_i + \lambda R_j) \to (R_i)$, $(R_i) \leftrightarrow (R_j)$, applied to the augmented matrix $\widetilde{A} = [A \mid b]$.

- **Multipliers** — at step $k$, $m_{i,k-1} = \dfrac{a^{(k-1)}_{i,k-1}}{a^{(k-1)}_{k-1,k-1}}$, $i = k, \dots, n$, used in $\left(R_i - \dfrac{a^{(k-1)}_{i,k-1}}{a^{(k-1)}_{k-1,k-1}} R_{k-1}\right) \to (R_i)$.

- **Pivot** — the diagonal element $a^{(i)}_{ii}$ used to eliminate below it (must be nonzero; ideally not small relative to the column).

- **Partial pivoting (maximal pivoting on columns)** — choose the largest absolute element below the diagonal as pivot: $|a^{(k)}_{pk}| = \max_{k \leq l \leq n}|a^{(k)}_{lk}|$. Guarantees $|m_{i,k}| \leq 1$, reducing loss-of-significance. Most popular in practice.

- **Scaled (partial) pivoting on columns** — define a scaling factor per row $s_i = \max_{j=\overline{1,n}}|a_{ij}|$ (or $s_i = \sum_j |a_{ij}|$); at step $i$ pick smallest $p$ with $\dfrac{|a_{pi}|}{s_i} = \max_{1 \leq j \leq n}\dfrac{|a_{ji}|}{s_j}$, then swap rows.

- **Total (maximal) pivoting** — find $|a_{pq}| = \max\{|a_{ij}|, i, j = \overline{k,n}\}$ and interchange both rows and columns ($(R_k)\leftrightarrow(R_p)$, $(C_k)\leftrightarrow(C_q)$); requires tracking the column (unknown) permutations.

- **LU factorization (decomposition)** — writing $A = LU$ with $L$ lower triangular and $U$ upper triangular.

- **Schur complement** — when partitioning $A = \begin{bmatrix} a_{11} & w^* \\ v & A' \end{bmatrix}$, the matrix $A' - vw^*/a_{11}$.

- **Doolittle / Crout factorizations** — LU is not unique; *Doolittle* imposes $l_{ii} = 1$ (unit lower triangular), *Crout* imposes $u_{ii} = 1$. The constructive proof of Theorem 1.7 yields Doolittle.

- **Diagonally dominant (on rows)** — $|a_{ii}| > \sum_{\substack{j=1 \\ j\neq i}}^{n}|a_{ij}|$, $i = \overline{1,n}$.

- **Positive definite** — $x^T A x > 0$ for all $x \neq 0$.

## Theorems & Results

### Theorem 5.1 (Properties of divided differences)
Divided differences satisfy:

**a)** $f[x_0, x_1, \dots, x_n] = \sum_{i=0}^{n} \dfrac{f(x_i)}{u'(x_i)} = \sum_{i=0}^{n} \dfrac{f(x_i)}{u_i(x_i)}$, where $u(x) = (x-x_0)(x-x_1)\cdots(x-x_n)$ and $u_i(x) = \dfrac{u(x)}{x - x_i}$.

**b) (Symmetry)** For any permutation $\{i_0, i_1, \dots, i_n\}$ of $\{0, 1, \dots, n\}$, $f[x_{i_0}, x_{i_1}, \dots, x_{i_n}] = f[x_0, x_1, \dots, x_n]$.

**c)** $f[x_0, \dots, x_n] = \dfrac{(Wf)(x_0, \dots, x_n)}{V(x_0, \dots, x_n)}$, where $Wf$ is the Vandermonde-type determinant with last column $f(x_i)$, and $V(x_0, \dots, x_n) = \prod_{0 \leq j < i \leq n}(x_i - x_j)$ is the Vandermonde determinant.

**d)** For $e_k(x) = x^k$, $k \geq 0$: $e_k[x_0, \dots, x_n] = \begin{cases} 0, & k < n \\ 1, & k = n \end{cases}$. More generally, for a degree-$k$ polynomial $P_k = a_0 + a_1 x + \cdots + a_k x^k$: $P_k[x_0, \dots, x_n] = \begin{cases} 0, & k < n \\ a_n, & k = n \end{cases}$.

**e) (Mean-value formula)** If $f \in C^n[a,b]$ with $[a,b]$ the smallest interval containing the distinct nodes, there exists $\xi_n \in (a,b)$ with
$$f[x_0, x_1, \dots, x_n] = \frac{1}{n!}f^{(n)}(\xi_n).$$

**Remark 5.2.** As a consequence of e), if $f \in C^n[a,b]$ and $\alpha \in [a,b]$, then $\lim_{x_0,\dots,x_n \to \alpha} f[x_0,\dots,x_n] = \frac{1}{n!}f^{(n)}(\alpha)$ — the computational formula for divided differences with multiple nodes.

### Pattern of forward differences (proved by induction)
$$\Delta^n f(x_i) = \sum_{k=0}^{n}(-1)^k \binom{n}{k} f_{n-k+i} = \sum_{k=0}^{n}(-1)^{n-k}\binom{n}{k} f_{k+i},$$
in particular $\Delta^n f(x_0) = \sum_{k=0}^{n}(-1)^{n-k}\binom{n}{k} f_k$.

### Proposition 5.6 (Finite ↔ divided differences for equally spaced nodes)
For $f : X \to \mathbb{R}$ with equally spaced nodes,
$$f[a, a+h, \dots, a+nh] = \frac{1}{n! h^n}\Delta^n f(a).$$

### Remark 1.2 (Determinant of a triangular matrix)
$\det(A) = a_{11}a_{22}\cdots a_{nn}$; hence a triangular matrix is nonsingular iff all diagonal entries are nonzero.

### Remark 1.4 (Gaussian elimination and solvability)
If $A$ is singular of rank $p-1$, at step $p$ the bottom rows become zero. If $a^{(i)}_{i,n+1} = b^{(i)}_i = 0$ for all $i = p, \dots, n$, the system is *compatible but undetermined* (infinitely many solutions); otherwise it is *incompatible* (no solution). So Gaussian elimination also discusses solvability.

### Theorem 1.7 (LU Factorization)
If no row interchanges are necessary during Gaussian elimination for solving $Ax = b$, then $A = LU$ with $L$ lower and $U$ upper triangular.
**Proof sketch:** Partition $A = \begin{bmatrix} a_{11} & w^* \\ v & A' \end{bmatrix}$ and factor
$$A = \begin{bmatrix} 1 & 0 \\ v/a_{11} & I_{n-1} \end{bmatrix}\begin{bmatrix} a_{11} & w^* \\ 0 & A' - vw^*/a_{11} \end{bmatrix}.$$
Recurse on the Schur complement $A' - vw^*/a_{11} = L'U'$, repeating until reaching a $1\times 1$ scalar.

**Remark 1.8.** If $A = LU$, solving $Ax = b$ reduces to two triangular solves: $Ly = b$ then $Ux = y$.

**Remark 1.10.** In Doolittle factorization, $U = [u_{ij}]$ is the upper triangular matrix from Gaussian elimination (without pivoting), $u_{ij} = a^{(i)}_{ij}$ for $i \leq j$, and $L = [l_{ij}]$ holds the multipliers $l_{ij} = m_{ij} = \dfrac{a^{(j)}_{ij}}{a^{(j)}_{jj}}$ for $i \geq j$. No row interchanges are needed when $A$ is diagonally dominant on rows or positive definite.

### Computational complexity
- **Gaussian elimination**: elimination costs $\sum_{k=1}^{n-1}[2(n-k)(n-k+1) + (n-k)] = \frac{n(n-1)(4n+7)}{6}$ flops, back substitution adds $n^2$, total $\dfrac{n(4n^2 + 9n - 7)}{6} = \mathcal{O}\!\left(\frac{2}{3}n^3\right)$.
- **Cramer's rule** (determinants via minors): $\mathcal{O}((n+1)!)$. For $n = 10$, Gaussian elimination ≈ 805 operations vs. Cramer ≈ 3,628,800 — Cramer's rule is *not* a practical computational method.

## Example Problems

### Example: Solving triangular systems (Example 1.3)
**a) Upper triangular** $\begin{bmatrix} 2 & 4 & 2 \\ 0 & -1 & 1 \\ 0 & 0 & -1\end{bmatrix}x = \begin{bmatrix} 8 \\ 0 \\ -1\end{bmatrix}$. By **backward substitution**: $x_3 = \frac{-1}{-1} = 1$, $x_2 = \frac{1}{-1}(0 - x_3) = 1$, $x_1 = \frac{1}{2}(8 - 4x_2 - 2x_3) = 1$, so $x = [1\ 1\ 1]^T$.

**b) Lower triangular** $\begin{bmatrix} 1 & 0 & 0 \\ 1/2 & 1 & 0 \\ 1/2 & 1 & 1\end{bmatrix}x = \begin{bmatrix} 8 \\ 4 \\ 3\end{bmatrix}$. By **forward substitution**: $x_1 = 8$, $x_2 = \frac{1}{1}(4 - \frac{1}{2}x_1) = 0$, $x_3 = \frac{1}{1}(3 - \frac{1}{2}x_1 - x_2) = -1$, so $x = [8\ 0\ -1]^T$. These exercise the substitution formulas (1.6)–(1.7).

### Example: Gaussian elimination with different pivoting (Example 1.5)
System $\begin{cases} x_1 - x_2 + x_3 = -1 \\ -2x_1 + 2x_2 + x_3 = 2 \\ -3x_1 - x_2 + 5x_3 = -5\end{cases}$, augmented matrix $\widetilde{A} = \begin{bmatrix} 1 & -1 & 1 & -1 \\ -2 & 2 & 1 & 2 \\ -3 & -1 & 5 & -5\end{bmatrix}$. All three pivoting strategies give the same solution $x = [1\ 2\ 0]^T$:
- **Partial pivoting**: column-1 largest is $-3$, swap $R_1 \leftrightarrow R_3$, eliminate, then $R_2 \leftrightarrow$ pivot $8/3$, back-substitute.
- **Scaled partial pivoting**: row sums $s = [3, 5, 9]$; ratios $[1/3, 2/5, 3/9] = [5/15, 6/15, 5/15]$, max is row 2, swap $R_1 \leftrightarrow R_2$; at step 2 swap $R_2 \leftrightarrow R_3$.
- **Total pivoting**: max overall is $5 = a_{33}$, swap $R_1\leftrightarrow R_3$ and $C_1\leftrightarrow C_3$ (new unknown $x' = [x_3\ x_2\ x_1]^T$), eliminate; final $x' = [0\ 2\ 1]^T \Rightarrow x = [1\ 2\ 0]^T$.

This illustrates the three pivoting definitions and Remark 1.4 (solvability via elimination).

### Example: LU decomposition (Example 1.9)
Solve $\begin{cases} 2x_1 + 4x_2 + 2x_3 = 8 \\ x_1 + x_2 + 2x_3 = 4 \\ x_1 + x_2 + x_3 = 3\end{cases}$ with $A = \begin{bmatrix} 2 & 4 & 2 \\ 1 & 1 & 2 \\ 1 & 1 & 1\end{bmatrix}$.
Using **Theorem 1.7** via Schur complements: $a_{11} = 2$, $v = [1\ 1]^T$, $w^* = [4\ 2]$, $A' = \begin{bmatrix} 1 & 2 \\ 1 & 1\end{bmatrix}$. First Schur complement $A' - vw^*/a_{11} = \begin{bmatrix} -1 & 1 \\ -1 & 0\end{bmatrix}$; continuing yields
$$L = \begin{bmatrix} 1 & 0 & 0 \\ 1/2 & 1 & 0 \\ 1/2 & 1 & 1\end{bmatrix}, \quad U = \begin{bmatrix} 2 & 4 & 2 \\ 0 & -1 & 1 \\ 0 & 0 & -1\end{bmatrix}.$$
Then (Remark 1.8) solve $Ly = b = [8\ 4\ 3]^T$ → $y = [8\ 0\ -1]^T$ (Example 1.3b), then $Ux = y$ → $x = [1\ 1\ 1]^T$ (Example 1.3a). This ties LU directly back to the two triangular-solve examples.

## Cheat-sheet
- Forward: $\Delta f_i = f_{i+1} - f_i$; backward: $\nabla f_i = f_i - f_{i-1}$.
- $\Delta^n f(x_0) = \sum_{k=0}^{n}(-1)^{n-k}\binom{n}{k}f_k$.
- Finite ↔ divided (equispaced): $f[a, a+h, \dots, a+nh] = \frac{1}{n!h^n}\Delta^n f(a)$.
- Mean value: $f[x_0,\dots,x_n] = \frac{1}{n!}f^{(n)}(\xi_n)$; symmetry: divided differences are permutation-invariant.
- Vandermonde form: $f[x_0,\dots,x_n] = \frac{(Wf)(\cdot)}{V(\cdot)}$, $V = \prod_{j<i}(x_i - x_j)$.
- Polynomial degree $k$: $P_k[x_0,\dots,x_n] = a_n$ if $k=n$, $0$ if $k<n$.
- Backward sub: $x_i = \frac{1}{u_{ii}}(b_i - \sum_{j>i}u_{ij}x_j)$; forward sub: $x_i = \frac{1}{l_{ii}}(b_i - \sum_{j<i}l_{ij}x_j)$.
- Gaussian multiplier $m_{i,k-1} = a^{(k-1)}_{i,k-1}/a^{(k-1)}_{k-1,k-1}$; partial pivoting ⇒ $|m_{i,k}| \leq 1$.
- Pivoting: partial (max in column), scaled (max ratio to row scale), total (max in submatrix, swaps rows + columns).
- $A = LU$ exists if no row interchanges needed; solve $Ly=b$ then $Ux=y$. Doolittle: $l_{ii}=1$; Crout: $u_{ii}=1$.
- No interchanges needed if $A$ diagonally dominant ($|a_{ii}| > \sum_{j\neq i}|a_{ij}|$) or positive definite ($x^TAx>0$).
- Cost: Gaussian elimination $\mathcal{O}(\frac{2}{3}n^3)$; Cramer's rule $\mathcal{O}((n+1)!)$ — impractical.
