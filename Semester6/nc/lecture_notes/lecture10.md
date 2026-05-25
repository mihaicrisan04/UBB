# Lecture 10 — Iterated Quadratures (Richardson, Romberg) and Gaussian Quadratures

## Overview
This lecture continues numerical integration with two themes. First, error-driven acceleration: Richardson extrapolation eliminates leading error terms to build high-accuracy estimates from low-order ones, and Romberg's method applies this systematically on top of the composite trapezoidal rule. Second, Gaussian quadratures, which choose both nodes and coefficients to integrate polynomials of as high a degree as possible exactly, achieving degree of precision $2m-1$ with $m$ nodes; the nodes turn out to be roots of orthogonal polynomials, computable as eigenvalues of the Jacobi matrix.

## Key Notions & Definitions
- **Richardson extrapolation** — given $I \approx I_n$ with asymptotic error $I - I_n \approx \frac{c}{n^p}$, combine $I_n$ and $I_{2n}$ to cancel $c$:
  $$I \approx \frac{2^p I_{2n} - I_n}{2^p - 1} = I_{2n} + \frac{I_{2n}-I_n}{2^p-1} \stackrel{\text{not}}{=} R_{2n}.$$
- **Richardson error estimate** — $I - I_{2n} \approx \frac{I_{2n}-I_n}{2^p-1}$ (a computable estimate not requiring $c$).
- **Order estimate from three values (3.9)** — using $I_n, I_{2n}, I_{4n}$,
  $$p \approx \log_2\!\left(\frac{I_{2n}-I_n}{I_{4n}-I_{2n}}\right) = \frac{1}{\ln 2}\ln\!\left(\frac{I_{2n}-I_n}{I_{4n}-I_{2n}}\right).$$
- **Iterated quadrature** — applies a composite rule, then halves the step at each level reusing previously computed function values; overcomes the recomputation cost of adaptive quadrature.
- **Romberg's method** — iterated algorithm starting from the composite trapezoidal (or midpoint) rule and accelerating it with repeated Richardson extrapolation. Steps use $n_k = 2^{k-1}$, $h_k = \frac{b-a}{2^{k-1}}$.
- **Romberg first column ($R_{k,1}$)** — repeated trapezoidal approximations, built recursively reusing prior values:
  $$R_{k,1} = \frac{1}{2}\left[R_{k-1,1} + h_{k-1}\sum_{i=1}^{2^{k-2}} f\!\Big(a + \big(i-\tfrac12\big)h_{k-1}\Big)\right], \quad k = \overline{2,n}.$$
- **Gaussian quadrature (Def. 3.7)** — an interpolatory formula $\int_a^b w(x) f(x)\,dx = \sum_{k=1}^m A_k f(x_k) + R_m(f)$ with maximum degree of precision $d = 2m-1$.
- **Moments** — $\mu_j = \int_a^b w(x) x^j\,dx$; must exist and be finite for all $j \in \mathbb{N}$ for $w$ to be a valid weight function.
- **Inner product induced by $w$** — $\langle f,g\rangle_w = \int_a^b w(x) f(x) g(x)\,dx$.
- **Jacobi matrix (3.29)** — symmetric tridiagonal $J_m(w)$ with diagonal $\alpha_0,\dots,\alpha_{m-1}$ and off-diagonals $\sqrt{\beta_1},\dots,\sqrt{\beta_{m-1}}$, from the three-term recurrence coefficients.

## Theorems & Results

### Richardson formulas for Newton–Cotes (Example 3.1)
- Composite **trapezoidal** has $I - T_n \approx \frac{c}{n^2}$, i.e. $p=2$. Extrapolation:
  $$I \approx \frac{4 T_{2n} - T_n}{3} = T_{2n} + \tfrac13(T_{2n}-T_n), \qquad I - T_{2n} \approx \tfrac13(T_{2n}-T_n).$$
- Composite **Simpson** has $p=4$:
  $$I \approx \frac{16 S_{2n} - S_n}{15} = S_{2n} + \tfrac{1}{15}(S_{2n}-S_n), \qquad I - S_{2n} \approx \tfrac{1}{15}(S_{2n}-S_n).$$

### Romberg table and recurrence (3.12)
For $f \in C^{2n+2}[a,b]$, applying Richardson repeatedly down the columns:
$$R_{k,j} = \frac{4^{j-1} R_{k,j-1} - R_{k-1,j-1}}{4^{j-1} - 1}, \qquad k = \overline{2,n},\; j = \overline{2,k}.$$
Arranged as a lower-triangular table; the first column $\{R_{n,1}\}$ is the repeated trapezoidal rule, the second column $\{R_{k,2}\}$ corresponds to composite Simpson, the third to Richardson-extrapolated Simpson, etc. Diagonal $\{R_{n,n}\}$ converges faster than the first column.
$$
\begin{array}{ccccc}
R_{1,1} & & & & \\
R_{2,1} & R_{2,2} & & & \\
R_{3,1} & R_{3,2} & R_{3,3} & & \\
\vdots & \vdots & \vdots & \ddots & \\
R_{n,1} & R_{n,2} & R_{n,3} & \cdots & R_{n,n}
\end{array}
$$
Stopping criterion: $|R_{n-1,n-1} - R_{n,n}| < \varepsilon$.

### Gaussian quadrature exactness conditions (3.16)
Nodes $x_k$ and coefficients $A_k$ are found by forcing $R_m(e_0) = R_m(e_1) = \dots = R_m(e_{2m-1}) = 0$, i.e. exactness for $1, x, \dots, x^{2m-1}$. This is a system of $2m$ equations in $2m$ unknowns, linear in the $A_k$ (a Vandermonde system) but nonlinear in the nodes — ill-conditioned and possibly with complex/out-of-range roots, motivating the orthogonal-polynomial approach.

### Orthogonality theorem (Theorem 3.11)
Let $u(x) = (x-x_1)(x-x_2)\cdots(x-x_m)$. The quadrature formula is exact for all $p \in \mathbb{P}_{2m-1}$ **if and only if** $u \perp \mathbb{P}_{m-1}$ with respect to $\langle \cdot,\cdot\rangle_w$.
Proof sketch: ($\Rightarrow$) for $p \in \mathbb{P}_{m-1}$, $up \in \mathbb{P}_{2m-1}$ is integrated exactly, and the sum $\sum A_k u(x_k) p(x_k) = 0$ since $u(x_k)=0$, so $\langle u,p\rangle_w = 0$. ($\Leftarrow$) write any $f \in \mathbb{P}_{2m-1}$ as $f = uq + r$ with $q,r \in \mathbb{P}_{m-1}$ (division algorithm); $\int w f = \int w u q + \int w r = 0 + \int w r$ since $u\perp q$, and the interpolatory rule (degree $\ge m-1$) is exact for $r$, with $f(x_k)=r(x_k)$, giving exactness for $f$.

### Consequences (Remark 3.12)
- Gaussian nodes are the roots of the polynomial orthogonal to $\mathbb{P}_{m-1}$ w.r.t. $w$; these are **real, distinct, and interior to $(a,b)$**.
- Nodes can be found from the determinant equation (3.25) involving the moments $\mu_0,\dots,\mu_{2m-1}$.
- A three-term recurrence holds for monic orthogonal polynomials:
  $$\pi_{k+1}(t) = (t - \alpha_k)\pi_k(t) - \beta_k \pi_{k-1}(t), \quad \pi_{-1}=0,\ \pi_0=1,$$
  with $\alpha_k = \frac{\langle t\pi_k, \pi_k\rangle}{\|\pi_k\|^2}$, $\beta_k = \frac{\|\pi_k\|^2}{\|\pi_{k-1}\|^2}$, $\beta_0 = \mu_0$.

### Positivity and convergence
- **Proposition 3.14:** all coefficients $A_k$ are positive.
- **Theorem 3.15:** if $[a,b]$ is bounded and $f \in C[a,b]$, the Gaussian formula converges, $R_m(f) \to 0$ as $m \to \infty$ (proof based on Weierstrass' theorem).
- **Proposition 3.16 (remainder):** if $f \in C^{2m}[a,b]$, there is $\xi \in (a,b)$ with
  $$R_m(f) = \frac{f^{(2m)}(\xi)}{(2m)!}\int_a^b w(x) u^2(x)\,dx.$$

### Jacobi matrix theorem (Theorem 3.18)
The nodes $\{x_k\}$ are the eigenvalues of the symmetric tridiagonal Jacobi matrix $J_m(w)$:
$$J_m v_k = x_k v_k, \quad v_k^T v_k = 1, \qquad A_k = \beta_0 v_{k,1}^2,$$
where $v_{k,1}$ is the first component of the normalized eigenvector for $x_k$. This reduces finding a Gaussian rule to a well-studied symmetric tridiagonal eigenvalue problem (Remark 3.19).

### Classical orthogonal polynomials (Table 3)
| Name | Notation | Polynomial | Weight | Interval | $\alpha_k$ | $\beta_k$ |
|---|---|---|---|---|---|---|
| Legendre | $l_m$ | $[(x^2-1)^m]^{(m)}$ | $1$ | $[-1,1]$ | $0$ | $\beta_0=2$, $\beta_k=(4-k^{-2})^{-1}$ |
| Chebyshev 1st | $T_m$ | $\cos(m\arccos x)$ | $(1-x^2)^{-1/2}$ | $[-1,1]$ | $0$ | $\beta_0=\pi$, $\beta_1=\tfrac12$, $\beta_k=\tfrac14\,(k\ge2)$ |
| Chebyshev 2nd | $Q_m$ | $\frac{\sin((m+1)\arccos x)}{\sqrt{1-x^2}}$ | $(1-x^2)^{1/2}$ | $[-1,1]$ | $0$ | $\beta_0=\tfrac{\pi}{2}$, $\beta_k=\tfrac14$ |
| Laguerre | $L_m^a$ | $x^{-a}e^x(x^{m+a}e^{-x})^{(m)}$ | $x^a e^{-x},\,a>-1$ | $[0,\infty)$ | $2k+a+1$ | $\beta_0=\Gamma(1+a)$, $\beta_k=k(k+a)$ |
| Hermite | $H_m$ | $(-1)^m e^{x^2}(e^{-x^2})^{(m)}$ | $e^{-x^2}$ | $\mathbb{R}$ | $0$ | $\beta_0=\sqrt{\pi}$, $\beta_k=\tfrac{k}{2}$ |

## Example Problems

### Example: Richardson on the composite trapezoidal rule for $\int_0^1 e^{-x^2}dx$ (Example 3.2)
Exact $I = 0.746824\ldots$, with $T_2 = 0.7313702518$, $T_4 = 0.7429840978$. Using the $p=2$ extrapolation formula above:
$$I \approx R_4 = \tfrac13(4T_4 - T_2) = 0.7468553798,$$
absolute error $3.12\mathrm{e}{-5}$ — better than $T_{32}$ (after 5 steps) using only 2 steps. The error estimate $I - T_4 \approx \tfrac13(T_4 - T_2) = 0.00387$ closely matches the true error $0.00384$. This applies **Richardson extrapolation** with the known order $p=2$ of the trapezoidal rule.

### Example: Estimating $p$ from three values (Example 3.4)
Using the trapezoidal values, formula (3.9) gives $p_1 = \log_2\!\frac{T_4-T_2}{T_8-T_4} = 2.0109$, $p_2 = 2.0028$, $p_3 = 2.0007$, converging to $p=2$ — the theoretical order of the composite trapezoidal rule. This validates the assumption $I - T_n \approx c/n^p$.

### Example: Romberg's method for $\int_0^\pi \sin x\,dx$ (Example 3.6)
Exact $I = 2$, precision $\varepsilon = 10^{-1}$. This exercises the **Romberg table** recurrence:
- $R_{1,1} = \tfrac{\pi}{2}(\sin 0 + \sin\pi) = 0$.
- $R_{2,1} = \tfrac12[R_{1,1} + h_1 f(a+\tfrac12 h_1)] = \tfrac{\pi}{2} = 1.5708$; $R_{2,2} = \tfrac{4R_{2,1}-R_{1,1}}{3} = 2.0944$. Since $|R_{2,2}-R_{1,1}|=2.0944 > 0.1$, continue.
- $R_{3,1} = 1.8961$, $R_{3,2} = \tfrac{4R_{3,1}-R_{2,1}}{3} = 2.0046$, $R_{3,3} = \tfrac{16R_{3,2}-R_{2,2}}{15} = 1.9986$. Now $|R_{3,3}-R_{2,2}| = 0.0958 < 0.1$.
Result $I \approx R_{3,3} = 1.9986$ (error $1.4\mathrm{e}{-3}$), beating trapezoidal $R_{3,1}$ (error $0.1039$) and 4-node Simpson ($2.005$, error $5\mathrm{e}{-3}$).

### Example: Gauss–Legendre 2-node formula (Examples on $[-1,1]$, Eq. 3.18–3.20)
Seek $\int_{-1}^1 f\,dx \approx A_1 f(x_1) + A_2 f(x_2)$ exact for $1,x,x^2,x^3$. The system gives
$$A_1 = A_2 = 1, \quad x_1 = -\tfrac{\sqrt3}{3},\; x_2 = \tfrac{\sqrt3}{3} \;\Rightarrow\; \int_{-1}^1 f\,dx \approx f\!\left(-\tfrac{\sqrt3}{3}\right) + f\!\left(\tfrac{\sqrt3}{3}\right).$$
Degree of exactness $d=3$ with only 2 nodes (Simpson needs 3 for the same). Via Theorem 3.11 / Example 3.13, the nodes are the roots of the Legendre polynomial $l_2(x) = 4(3x^2-1)$, i.e. $\pm\tfrac{\sqrt3}{3}$.

### Example: Gaussian rule with a singular weight, 1 node (Example 3.9)
Find $\int_0^1 \frac{f(x)}{\sqrt x}\,dx \approx A_1 f(x_1)$ with maximal precision. Here $w(x)=x^{-1/2}$ has finite moments $\mu_j = \frac{2}{2j+1}$. Forcing exactness for $e_0,e_1$: $A_1 = \mu_0 = 2$, $A_1 x_1 = \mu_1 = \tfrac23 \Rightarrow x_1 = \tfrac13$. Thus $\int_0^1 \frac{f(x)}{\sqrt x}\,dx \approx 2f(\tfrac13)$, $d=1$. Demonstrates the **weight function absorbing the $1/\sqrt x$ singularity**.

### Example: Gauss–Laguerre 2-node formula (Example 3.10)
Weight $w(x)=e^{-x}$ on $[0,\infty)$, moments $\mu_j = \Gamma(j+1) = j!$. Seeking exactness for $1,x,x^2,x^3$ yields
$$\int_0^\infty e^{-x} f(x)\,dx \approx \tfrac{2+\sqrt2}{4} f(2-\sqrt2) + \tfrac{2-\sqrt2}{4} f(2+\sqrt2),$$
$d=3$. The nodes $2\pm\sqrt2$ are the roots of the Laguerre polynomial $L_2^0(x) = x^2 - 4x + 2$ (Example 3.13), consistent with Theorem 3.11.

### Example: Remainder of 2-node Gauss formulas (Example 3.17)
Using Proposition 3.16:
- Gauss–Legendre, $u(x) = x^2 - \tfrac13$: $R_2(f) = \tfrac{f^{(4)}(\xi)}{4!}\int_{-1}^1 (x^2-\tfrac13)^2\,dx = \tfrac{1}{135} f^{(4)}(\xi)$.
- Gauss–Laguerre, $u(x) = (x-2)^2 - 2 = x^2 - 4x + 2$: $R_2(f) = \tfrac{f^{(4)}(\xi)}{4!}\int_0^\infty e^{-x}(x^2-4x+2)^2\,dx = \tfrac16 f^{(4)}(\xi)$ (using $\int_0^\infty e^{-x}x^k\,dx = k!$).

## Cheat-sheet
- Richardson: $I \approx \frac{2^p I_{2n} - I_n}{2^p-1}$; error $I - I_{2n} \approx \frac{I_{2n}-I_n}{2^p-1}$.
- Trapezoidal $p=2$: $I \approx \frac{4T_{2n}-T_n}{3}$. Simpson $p=4$: $I \approx \frac{16S_{2n}-S_n}{15}$.
- Order check: $p \approx \log_2\frac{I_{2n}-I_n}{I_{4n}-I_{2n}}$.
- Romberg recurrence: $R_{k,j} = \frac{4^{j-1}R_{k,j-1} - R_{k-1,j-1}}{4^{j-1}-1}$; stop when $|R_{n-1,n-1}-R_{n,n}|<\varepsilon$. Column 1 = trapezoidal, column 2 = Simpson.
- Gaussian quadrature: $m$ nodes give degree of precision $2m-1$; coefficients $A_k > 0$.
- Nodes = roots of orthogonal polynomial $u \perp \mathbb{P}_{m-1}$ w.r.t. $\langle f,g\rangle_w = \int_a^b w f g\,dx$; nodes real, distinct, in $(a,b)$.
- Gauss remainder: $R_m(f) = \frac{f^{(2m)}(\xi)}{(2m)!}\int_a^b w(x) u^2(x)\,dx$.
- Jacobi matrix: nodes = eigenvalues of $J_m(w)$ (symmetric tridiagonal, diag $\alpha_k$, off-diag $\sqrt{\beta_k}$); $A_k = \beta_0 v_{k,1}^2$.
- Three-term recurrence: $\pi_{k+1} = (t-\alpha_k)\pi_k - \beta_k\pi_{k-1}$.
- Gauss–Legendre 2-node: $\int_{-1}^1 f \approx f(-\tfrac{\sqrt3}{3}) + f(\tfrac{\sqrt3}{3})$, $d=3$, $R_2 = \tfrac{1}{135}f^{(4)}(\xi)$.
- Map $[-1,1]\to[a,b]$: $x = \frac{b+a+t(b-a)}{2}$.
- Families: Legendre ($w=1$), Chebyshev ($w=(1-x^2)^{\mp1/2}$), Laguerre ($w=x^a e^{-x}$, $[0,\infty)$), Hermite ($w=e^{-x^2}$, $\mathbb{R}$).
