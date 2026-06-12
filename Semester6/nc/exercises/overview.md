# NC Exercises — Overview & Index

A map of every worked exercise in this folder, grouped by topic, with links to the full explanations. Two big themes run through the course:

- **Approximation of functions** — represent or replace $f$ by something simpler: interpolation (exact at nodes), least squares (best non-exact fit), and numerical integration (quadrature, usually built from an interpolant).
- **Solving equations** — linear systems $Ax = b$ (direct & iterative), and nonlinear equations via iteration $x_{n+1} = g(x_n)$.

For the condensed step-by-step recipes see [cheatsheet.md](cheatsheet.md) (printable: [cheatsheet.html](cheatsheet.html)).

---

## 1. Interpolation — polynomial through given data

Build a polynomial that matches $f$ **exactly** at the nodes. Which method depends on *what* data you're given at each node.

| What you're given | Method | File |
|---|---|---|
| function values $f(x_i)$ at distinct nodes | **Newton / Lagrange** | [newton-lagrange-example.md](newton-lagrange-example.md) |
| function values **and** consecutive derivatives at nodes ($f, f', \dots$) | **Hermite** | [hermite-interpolation-example.md](hermite-interpolation-example.md) |
| value at one node, derivative at another — with a "gap" (e.g. $f(-1)$ and $f'(1)$) | **Birkhoff** | [birkhoff-quadrature-peano-example.md](birkhoff-quadrature-peano-example.md) (part a) |

- **Newton vs Lagrange** — same unique polynomial, two forms. Newton uses the divided-difference table (incremental, easy to extend); Lagrange uses basis polynomials $\ell_i$.
- **Hermite** — repeat each node by its multiplicity in the divided-difference table and **seed** all-same-node brackets with derivatives: $f[\underbrace{x,\dots,x}_{k+1}] = \tfrac{f^{(k)}(x)}{k!}$.
- **Birkhoff** — the "gappy" value/derivative pattern. Small enough to solve directly for the coefficients of $a + bx + \dots$. (The "B" method — not Cholesky.)

> Degree rule for all three: $\deg = (\text{number of conditions}) - 1$.

---

## 2. Least-squares approximation — best non-exact fit

Used when there are **more data points than a degree-$k$ polynomial can pass through** ("best fit", "least squares", "approximate"). The fitted curve deliberately does **not** pass through the points; it minimizes the sum of squared errors via the **normal equations**.

| Problem | File |
|---|---|
| fit a line (degree 1) to several points | [least-squares-linear-fit-example.md](least-squares-linear-fit-example.md) |

Key contrast with §1: interpolation is exact at the nodes; least squares is a compromise. Don't use a divided-difference table here.

---

## 3. Numerical integration (quadrature) + Peano remainder

Approximate $\int_a^b w(x) f(x)\,dx$ by a weighted sum of $f$-values (and possibly $f'$-values). Two recurring tasks: **find the formula** (max degree of precision) and **find the remainder** $R(f)$ (Peano).

| Problem | File |
|---|---|
| 1-point weighted rule, max degree of precision (weight $w(x)=x^2$) | [weighted-quadrature-1point-example.md](weighted-quadrature-1point-example.md) |
| 1-point weighted rule, max degree of precision (weight $w(x)=\lvert x\rvert$) | [weighted-quadrature-1point-example-2.md](weighted-quadrature-1point-example-2.md) |
| quadrature obtained by **integrating an interpolant**, + degree of precision + Peano remainder | [birkhoff-quadrature-peano-example.md](birkhoff-quadrature-peano-example.md) (parts b, c) |
| Birkhoff interpolant → quadrature, **max-DoP node/weight determination**, + Peano (DoP 2) | [birkhoff-quadrature-maxdop-example.md](birkhoff-quadrature-maxdop-example.md) |

- **Max degree of precision** — fix the weights/nodes by demanding exactness on $1, x, x^2, \dots$; use the moments $\mu_k = \int_a^b w(x) x^k\,dx$.
- **Peano remainder** — when the kernel $K(t)$ keeps its sign, $R(f) = \tfrac{R(x^{d+1})}{(d+1)!} f^{(d+1)}(\xi)$, where $d$ is the degree of precision. Shared technique with derivation: [peano-remainder-shortcut.md](peano-remainder-shortcut.md).
- **Link to §1:** the third file shows the standard pipeline *interpolate → integrate the interpolant → quadrature formula → remainder*, tying interpolation and integration together.

---

## 4. Nonlinear equations & iteration ($x_{n+1} = g(x_n)$)

The "recurrence" exercises: given an iteration with a fixed point $\alpha$, study **convergence**, the **order of convergence**, and an **error bound**. Core rule: order $p$ = index of the first non-vanishing derivative of $g$ at $\alpha$; constant $\tfrac{g^{(p)}(\alpha)}{p!}$.

| Problem | Order | File |
|---|---|---|
| existence/uniqueness via contraction + linear convergence (e.g. $x = \tfrac{1+\sin x}{2}$) | 1 (linear) | [fixed-point-contraction-example.md](fixed-point-contraction-example.md) |
| Newton's method for $\sqrt{a}$ (e.g. $\sqrt5$) | 2 (quadratic) | [newton-sqrt-convergence-example.md](newton-sqrt-convergence-example.md) |
| fixed-point iteration with extra vanishing derivatives | 3 (cubic) | [fixed-point-cubic-convergence-example.md](fixed-point-cubic-convergence-example.md) |
| parametrized iteration $g(x; c)$ — order depends on $c$ ($\alpha=1$) | 1 or 2 | [parametrized-iteration-convergence-example.md](parametrized-iteration-convergence-example.md) |
| parametrized iteration $g(x; c)$ — order depends on $c$ ($\alpha=2$) | 1 or 2 | [parametrized-iteration-convergence-example-2.md](parametrized-iteration-convergence-example-2.md) |

- **Convergence:** local convergence near $\alpha$ needs $|g'(\alpha)| < 1$.
- **Order jump:** wherever $g'(\alpha) = 0$ the order rises from linear (1) to at least quadratic (2); more vanishing derivatives → higher order.
- **Error bound:** linear → $|e_n| \le \lambda^n |e_0|$; order $p$ → $|e_{n+1}| \le \tfrac{M_p}{p!}|e_n|^p$.

---

## 5. Linear systems $Ax = b$ (already covered — for completeness)

| Method | When | File |
|---|---|---|
| **Gaussian elimination** | simplest direct solve | [gaussian-elimination-example.md](gaussian-elimination-example.md) |
| **LUP factorization** | general matrix, partial pivoting; reuse for many $b$ | [lup-factorization-example.md](lup-factorization-example.md) |
| **Cholesky** | symmetric positive definite $A$ | [cholesky-solve-example.md](cholesky-solve-example.md) |
| **Jacobi** | iterative; diagonally dominant $A$ | [jacobi-iteration-example.md](jacobi-iteration-example.md) |
| **Gauss–Seidel** | iterative; reuses fresh components each sweep | [gauss-seidel-iteration-example.md](gauss-seidel-iteration-example.md) |

---

## Quick "which kind is this?" guide

- *"Find the polynomial through these points"* → §1 interpolation (check: values only / +derivatives / gappy).
- *"Best fit / least squares / approximate these points"* → §2 least squares.
- *"Find a quadrature formula / degree of precision / remainder $R(f)$"* → §3 integration.
- *"Iteration $x_{n+1} = \dots$ / order of convergence / approximate $\sqrt{\cdot}$ or a root"* → §4 nonlinear iteration.
- *"Solve the system $Ax = b$ / factorize $A$"* → §5 linear systems.
