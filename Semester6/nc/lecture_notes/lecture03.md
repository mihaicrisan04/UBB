# Lecture 3 — Factorization Methods (LUP, QR, Cholesky), Iterative Methods & Conditioning

## Overview
This lecture finishes the factorization-based direct solvers (LUP with pivoting, QR, Cholesky) and then introduces iterative methods (Jacobi, Gauss-Seidel, SOR) together with the general fixed-point theory needed to analyze their convergence. It closes with the conditioning of a linear system, which measures how sensitive the solution of $Ax=b$ is to perturbations in the data.

## Key Notions & Definitions

- **LUP factorization (decomposition)** — for a matrix $A$ that needs row interchanges (pivoting), a triplet $(L, U, P)$ with $L$ lower triangular, $U$ upper triangular and $P$ a permutation matrix such that $$PA = LU.$$ Solving $Ax=b$ then reduces to two triangular systems: $Ly = Pb$ and $Ux = y$.

- **Permutation matrix** — a matrix obtained from the identity $I$ by permuting rows. Left-multiplication by $P$ performs row interchanges; right-multiplication performs the same column interchanges.

- **Banded matrix** — a sparse matrix whose nonzero entries lie in a diagonal band: $a_{ij}=0$ if $j < i - k_1$ or $j > i + k_2$, where $k_1,k_2\ge 0$ are the lower/upper bandwidths. The **bandwidth** is $w = \max\{k_1,k_2\}$, i.e. $a_{ij}=0$ if $|i-j| > w$. A **tridiagonal** matrix has $w=1$.

- **Orthogonal matrix** — a real square matrix $Q$ with $$Q\cdot Q^T = Q^T\cdot Q = I,$$ hence nonsingular with $Q^{-1}=Q^T$. Orthogonal matrices preserve lengths and angles and do not magnify errors.

- **QR factorization** — for a real square $A$, the pair $(Q,R)$ with $Q$ orthogonal and $R$ upper triangular with $r_{ii}>0$ such that $A = QR$. Solving $Ax=b$ becomes the triangular system $Rx = Q^Tb$.

- **Positive definite matrix** — a real square matrix $A$ with $$x^T A x = \sum_{i,j=1}^n a_{ij}x_i x_j > 0,\quad \forall x\in\mathbb{R}^n,\ x\neq 0.$$ A symmetric matrix is positive definite iff all its eigenvalues are real and positive; eigenvectors for distinct eigenvalues are orthogonal.

- **Cholesky factorization** — for a symmetric positive definite $A$, the unique decomposition $$A = R^T R,$$ with $R$ upper triangular and $r_{ii}>0$.

- **Matrix norm** — a map $\|\cdot\|:\mathbb{R}^{n\times n}\to\mathbb{R}$ satisfying, for all $A,B$ and scalars $\alpha$: (i) $\|A\|\ge 0$, $\|A\|=0\Leftrightarrow A=0$; (ii) $\|\alpha A\|=|\alpha|\,\|A\|$; (iii) $\|A+B\|\le\|A\|+\|B\|$; (iv) $\|AB\|\le\|A\|\,\|B\|$ (the submultiplicative property, specific to matrix norms).

- **Natural (subordinate / induced) matrix norm** — associated with a vector norm $\|\cdot\|$: $$\|A\| = \sup_{v\neq 0}\frac{\|Av\|}{\|v\|} = \sup_{\|v\|\le 1}\|Av\| = \sup_{\|v\|=1}\|Av\|.$$ For any induced norm, $\|I\|=1$.

- **Eigen-notions** — for $A\in\mathbb{R}^{n\times n}$: characteristic polynomial $p(\lambda)=\det(A-\lambda I_n)$; eigenvalues are its roots; an eigenvector satisfies $(A-\lambda I_n)x=0$, $x\neq 0$; the spectrum $\lambda(A)$ is the set of eigenvalues; the **spectral radius** is $\rho(A)=\max\{|\lambda| : \lambda\in\lambda(A)\}$; the **trace** is $tr(A)=a_{11}+\dots+a_{nn}$.

- **Splitting of $A$** — a decomposition $A = M - N$ used to build iterative methods. With $M$ easily invertible, $Ax=b\Leftrightarrow x = M^{-1}Nx + M^{-1}b$, of fixed-point form $x = Tx + c$ with $T = M^{-1}N = I - M^{-1}A$, $c = M^{-1}b$.

- **Diagonally dominant matrix** — $|a_{ii}| > \sum_{j\neq i}|a_{ij}|$ for all $i$. Guarantees convergence of Jacobi (and Gauss-Seidel) iteration.

- **Condition number of $A$** — $$\mathrm{cond}(A) = \|A\|\,\|A^{-1}\|,$$ with $\mathrm{cond}(A)=\infty$ if $A$ is singular. It measures sensitivity of $x$ to relative perturbations in $b$; always $\mathrm{cond}(A)\ge 1$.

## Theorems & Results

### Remark 1.3 — cost of LU/LUP and banded matrices
LU/LUP factorization costs about the same as Gaussian elimination, $O(n^3)$ flops. For tridiagonal matrices this drops to $O(n)$ (the **Thomas algorithm**, based on LUP, stores only three diagonals). For banded matrices of bandwidth $w$, LU with partial pivoting needs $O(w^2 n)$ flops and triangular solves $O(wn)$ flops.

### Theorem 1.5 — existence and uniqueness of QR
Let $A$ be a real square matrix. Then there exist unique matrices $Q$ (orthogonal) and $R$ (upper triangular with $r_{ii}>0$) such that $A=QR$.

### Theorem 1.8 — Cholesky factorization
Let $A$ be symmetric positive definite. Then $A$ has a unique Cholesky factorization $A=R^TR$ with $R$ upper triangular, $r_{ii}>0$.

**Proof sketch.** Taking $x=e_1=[1\ 0\ \dots\ 0]^T$ in the positive-definiteness condition gives $x^TAx = a_{11} > 0$, so set $\alpha=\sqrt{a_{11}}$. Writing $A = \begin{bmatrix} a_{11} & w^T \\ w & A'\end{bmatrix}$, factor
$$A = \begin{bmatrix}\alpha & 0\\ w/\alpha & I_{n-1}\end{bmatrix}\begin{bmatrix}1 & 0\\ 0 & A'-ww^T/a_{11}\end{bmatrix}\begin{bmatrix}\alpha & w^T/\alpha\\ 0 & I_{n-1}\end{bmatrix} = R_1^T A_1 R_1.$$
By induction every matrix appearing (the Schur complements) stays positive definite, so the process cannot break down; repeating gives $A = R^T R$. Uniqueness follows because at each step $\alpha=\sqrt{a_{11}}$ is uniquely determined.

### Remark 1.9 — Cholesky cost
Cholesky needs only $n(n+1)/2$ storage locations (half the matrix) and about $O\!\left(\tfrac13 n^3\right)$ operations, vs $O\!\left(\tfrac23 n^3\right)$ for ordinary LU.

### Lemma 2.6 — Geometric series
Let $X\in\mathbb{R}^{n\times n}$. If $\rho(X)<1$, then $(I-X)^{-1}$ exists and $$(I-X)^{-1} = I + X + \dots + X^k + \dots.$$ Conversely, if the series converges, then $\rho(X)<1$.

### Theorem 2.7 — equivalent convergence conditions
For the iteration $x^{(k+1)} = Tx^{(k)} + c$, the following are equivalent: (a) the method converges; (b) $\rho(T)<1$; (c) $\|T\|<1$ for some matrix norm.

### Theorem 2.8 — convergence and error bounds
If $\|T\|<1$ for some matrix norm, then $\{x^{(k)}\}$ converges to the unique fixed point $x^*$ from any $x^{(0)}$, with the a posteriori and a priori error bounds
$$\|x^*-x^{(k)}\| \le \frac{\|T\|}{1-\|T\|}\|x^{(k)}-x^{(k-1)}\|,$$
$$\|x^*-x^{(k)}\| \le \frac{\|T\|^k}{1-\|T\|}\|x^{(1)}-x^{(0)}\| \le \frac{\|T\|}{1-\|T\|}\|x^{(1)}-x^{(0)}\|.$$

### Remark 2.9 — stopping criterion
For a target error $\varepsilon$, iterate until $\|x^{(k)}-x^{(k-1)}\| \le \frac{1-\|T\|}{\|T\|}\varepsilon$. In particular if $\|T\|<1/2$, then $\|x^*-x^{(k)}\| \le \|x^{(k)}-x^{(k-1)}\|$ and the criterion simplifies to $\|x^{(k)}-x^{(k-1)}\|\le\varepsilon$.

### Jacobi and Gauss-Seidel via $A = D - L - U$
With $D$ the diagonal, $-L$ the strictly lower and $-U$ the strictly upper parts:
- **Jacobi**: $M=D$, $N=L+U$, $T_J = D^{-1}(L+U)$, $c_J=D^{-1}b$. Component form $$x_i^{(k+1)} = \frac{1}{a_{ii}}\Big[b_i - \sum_{j\neq i} a_{ij}x_j^{(k)}\Big],\quad i=\overline{1,n}.$$ Converges if $\|T_J\|_\infty = \max_i \sum_{j\neq i}|a_{ij}/a_{ii}| < 1$, i.e. for diagonally dominant $A$; necessary and sufficient condition is $\rho(T_J)<1$.
- **Gauss-Seidel**: $M=D-L$, $N=U$, $T_{GS}=(D-L)^{-1}U$, $c_{GS}=(D-L)^{-1}b$. Component form $$x_i^{(k+1)} = \frac{1}{a_{ii}}\Big[b_i - \sum_{j=1}^{i-1} a_{ij}x_j^{(k+1)} - \sum_{j=i+1}^{n} a_{ij}x_j^{(k)}\Big].$$ For diagonally dominant $A$, $\|T_{GS}\|<1$ and Gauss-Seidel converges at least as fast as Jacobi.

### SOR (successive over-relaxation) and Theorem 2.10 (Ostrowski-Reich)
Introduce a relaxation parameter $\omega$: $M = \tfrac{D}{\omega}-L$, $N = \left(\tfrac{1-\omega}{\omega}D+U\right)$, giving
$$x_i^{(k+1)} = \frac{\omega}{a_{ii}}\Big[b_i - \sum_{j=1}^{i-1}a_{ij}x_j^{(k+1)} - \sum_{j=i+1}^n a_{ij}x_j^{(k)}\Big] + (1-\omega)x_i^{(k)}.$$
$\omega<1$ is sub-relaxation, $\omega=1$ is Gauss-Seidel, $\omega>1$ is over-relaxation (SOR). Since $\rho(T_\omega)\ge|\omega-1|$, a necessary condition for convergence is $0<\omega<2$. **Ostrowski-Reich:** if $A$ is positive definite and $0<\omega<2$, SOR converges for any $x^{(0)}$. The optimal value is $$\omega^* = \frac{2}{1+\sqrt{1-(\rho(T_J))^2}}.$$

### Conditioning of a linear system
For a general problem $y=f(x)$, $f:\mathbb{R}^m\to\mathbb{R}^n$, the conditioning matrix has entries $\gamma_{ij} = \dfrac{x_i\,\partial f_j/\partial x_i}{f_j(x)}$ and the condition number is $(\mathrm{cond}\,f)(x) = \|\Gamma(x)\|$. For the linear system $x = f(b) = A^{-1}b$, since $\partial f/\partial b = A^{-1}$,
$$\max_{b\neq 0}(\mathrm{cond}\,f)(b) = \|A^{-1}\|\,\|A\| = \mathrm{cond}(A).$$
Always $1 = \|I\| = \|AA^{-1}\| \le \|A\|\,\|A^{-1}\| = \mathrm{cond}(A)$. If $\mathrm{cond}(A)\approx 1$, small relative perturbations in $b$ give small relative perturbations in $x$; if $\mathrm{cond}(A)$ is large, $A$ is **ill-conditioned**.

## Example Problems

### Example: LUP factorization of a $3\times 3$ matrix (Example 1.2)
Find an LUP factorization of $A = \begin{bmatrix} 2 & 1 & -2 \\ 1 & 1 & -1 \\ 3 & -1 & 1\end{bmatrix}$.

**Solution.** Partial pivoting swaps $(R_1)\leftrightarrow(R_3)$ so the largest first-column entry ($3$) is the pivot. Eliminating below it gives multipliers $1/3$ and $2/3$, and the Schur complement $\begin{bmatrix}1&-1\\1&-2\end{bmatrix} - \tfrac13\begin{bmatrix}1\\2\end{bmatrix}[-1\ 1] = \begin{bmatrix}4/3 & -4/3\\ 5/3 & -8/3\end{bmatrix}$. A further interchange $(R_2)\leftrightarrow(R_3)$ and elimination (multiplier $4/5$, last Schur complement $4/5$) yields
$$L = \begin{bmatrix}1 & 0 & 0\\ 2/3 & 1 & 0\\ 1/3 & 4/5 & 1\end{bmatrix},\quad U = \begin{bmatrix}3 & -1 & 1\\ 0 & 5/3 & -8/3\\ 0 & 0 & 4/5\end{bmatrix},\quad P = \begin{bmatrix}0&0&1\\1&0&0\\0&1&0\end{bmatrix},$$
with $PA = LU$. This illustrates the **LUP factorization** definition above: the permutation $P$ records the two row interchanges.

### Example: Cholesky factorization (Example 1.10)
Find the Cholesky factorization of $A = \begin{bmatrix}4 & 12 & -16\\ 12 & 37 & -43\\ -16 & -43 & 98\end{bmatrix}$.

**Solution.** $A$ is symmetric and its eigenvalues $0.0188,\,15.5040,\,123.4772$ are real and positive, so $A$ is positive definite and Theorem 1.8 applies. Working on the lower triangle: $\alpha=\sqrt{4}=2$, first column of $R^T$ becomes $(2,6,-8)^T$. First Schur complement $\begin{bmatrix}37&-43\\-43&98\end{bmatrix}-\begin{bmatrix}6\\-8\end{bmatrix}[6\ -8] = \begin{bmatrix}1&5\\5&34\end{bmatrix}$, giving $\sqrt{1}=1$ and column $(1,5)$. Last Schur complement $34 - 5\cdot 5 = 9$, square root $3$. Hence
$$R^T = \begin{bmatrix}2&0&0\\6&1&0\\-8&5&3\end{bmatrix},\quad R = \begin{bmatrix}2&6&-8\\0&1&5\\0&0&3\end{bmatrix},\quad A = R^TR.$$
This directly executes the proof sketch of Theorem 1.8 (repeated Schur complements with $\alpha=\sqrt{a_{11}}$).

### Example: Jacobi vs Gauss-Seidel iteration (Section 2.1)
Solve the diagonally dominant system
$$9x_1 + x_2 + x_3 = b_1,\quad 2x_1 + 10x_2 + 3x_3 = b_2,\quad 3x_1 + 4x_2 + 11x_3 = b_3,$$
with $b = [10,19,0]^T$ (true solution $x=[1,2,-1]^T$), starting from $x^{(0)}=[0,0,0]^T$.

**Solution.** Jacobi uses $x_i^{(k+1)} = \frac{1}{a_{ii}}[b_i - \sum_{j\neq i}a_{ij}x_j^{(k)}]$; the error $\max_i|x_i - x_i^{(k)}|$ shrinks geometrically, with the error ratio approaching the limiting constant $\approx 0.447$ (so $\rho(T_J)\approx 0.447$). Gauss-Seidel reuses already-updated components within the same sweep and reaches the same solution in far fewer iterations (error $\sim 2.6\text{e}{-}6$ after 6 steps vs Jacobi needing ~30 for $\sim 10^{-11}$). Since $A$ is diagonally dominant, the convergence guarantee from the Jacobi/Gauss-Seidel discussion above holds, and the observed limiting Jacobi ratio is exactly $\rho(T_J)$ from Theorem 2.7.

### Example: Ill-conditioned matrices (Example 3.1)
The **Hilbert matrix** $H_n = \left[\frac{1}{i+j-1}\right]_{i,j=\overline{1,n}}$ is symmetric positive definite (hence nonsingular) yet severely ill-conditioned: $\mathrm{cond}_2(H_{10})\approx 1.6\text{e}{+}13$, $\mathrm{cond}_2(H_{20})\approx 2.45\text{e}{+}28$, $\mathrm{cond}_2(H_{40})\approx 7.65\text{e}{+}58$. The **Vandermonde matrix** with $t_i = 1/i$ satisfies $\mathrm{cond}_\infty(V_n) > n^{n+1}$. These illustrate the condition-number definition: although both matrices are invertible, their huge $\mathrm{cond}(A)$ means tiny perturbations in $b$ can produce enormous errors in $x$.

## Cheat-sheet
- LUP: $PA = LU$; solve $Ly=Pb$ then $Ux=y$.
- Tridiagonal solve (Thomas): $O(n)$; banded bandwidth $w$: $O(w^2 n)$ to factor.
- Orthogonal: $Q^T = Q^{-1}$, preserves lengths/angles.
- QR: $A=QR$ ($Q$ orthogonal, $R$ upper-triangular, $r_{ii}>0$); solve $Rx=Q^Tb$.
- Cholesky (SPD): $A=R^TR$, $\sim\tfrac13 n^3$ ops, half storage.
- Matrix norms: $\|A\|_1=\max_j\sum_i|a_{ij}|$, $\|A\|_\infty=\max_i\sum_j|a_{ij}|$, $\|A\|_2=\sqrt{\rho(A^TA)}$, Frobenius $\|A\|_F=\sqrt{tr(A^TA)}$ (nonsubordinate, $\|I_n\|_F=\sqrt{n}$).
- Iteration $x^{(k+1)}=Tx^{(k)}+c$ converges $\iff \rho(T)<1 \iff \|T\|<1$ for some norm.
- $A=D-L-U$: Jacobi $T_J=D^{-1}(L+U)$; Gauss-Seidel $T_{GS}=(D-L)^{-1}U$.
- Diagonal dominance $|a_{ii}|>\sum_{j\neq i}|a_{ij}|$ $\Rightarrow$ Jacobi & Gauss-Seidel converge.
- SOR needs $0<\omega<2$; $\omega^* = 2/\big(1+\sqrt{1-\rho(T_J)^2}\big)$.
- Error bound: $\|x^*-x^{(k)}\|\le \frac{\|T\|}{1-\|T\|}\|x^{(k)}-x^{(k-1)}\|$.
- $\mathrm{cond}(A)=\|A\|\,\|A^{-1}\|\ge 1$; large $\Rightarrow$ ill-conditioned (Hilbert, Vandermonde).
