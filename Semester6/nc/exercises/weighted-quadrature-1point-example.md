# 1-Point Weighted Quadrature (Max Degree of Precision) — Worked Example

**Problem.** Determine a 1-point quadrature formula

$$\int_{-1}^{1} x^2 f(x)\,dx = A_0\, f(x_0) + R(f)$$

with the maximum degree of precision possible. For $f \in C^2[-1,1]$, find an expression for the remainder $R(f)$.

## Setup

This is a weighted quadrature with weight $w(x) = x^2$ on $[-1,1]$. A 1-point rule has **2 free parameters** ($A_0, x_0$), so the maximum achievable degree of precision is $2n - 1 = 1$ (the Gaussian bound for $n = 1$).

Impose exactness on the basis $\{1, x\}$, using the moments of $w(x) = x^2$:

$$\mu_0 = \int_{-1}^{1} x^2\,dx = \frac{2}{3}, \qquad \mu_1 = \int_{-1}^{1} x^2 \cdot x\,dx = \int_{-1}^{1} x^3\,dx = 0.$$

- $f = 1$: $\quad A_0 = \mu_0 = \dfrac{2}{3}.$
- $f = x$: $\quad A_0 x_0 = \mu_1 = 0 \ \Rightarrow\ x_0 = 0$ (since $A_0 \neq 0$).

## The formula

$$\boxed{\ \int_{-1}^{1} x^2 f(x)\,dx = \frac{2}{3}\, f(0) + R(f)\ }$$

**Degree of precision check:** exact for $1$ and $x$. For $x^2$: LHS $= \int_{-1}^{1} x^4\,dx = \tfrac{2}{5}$, but RHS $= \tfrac{2}{3} f(0) = 0$. It fails at degree 2, so the **degree of precision is exactly $1$** — the maximum.

## Remainder for $f \in C^2[-1,1]$

Since the degree of precision is $d = 1$, the remainder involves $f''$. The Peano kernel of the functional $R$ is

$$K_1(t) = R_x\big[(x - t)_+\big] = \int_{t}^{1} x^2 (x - t)\,dx - \frac{2}{3}(0 - t)_+ = \frac{1}{4} - \frac{|t|}{3} + \frac{t^4}{12}.$$

On $[-1,1]$ this kernel is **$\ge 0$ and does not change sign** (it decreases from $K_1(0) = \tfrac14$ to $K_1(\pm 1) = 0$, since $\tfrac{d}{ds}\big(\tfrac14 - \tfrac{s}{3} + \tfrac{s^4}{12}\big) = \tfrac{s^3 - 1}{3} \le 0$ for $s = |t| \in [0,1]$). So the mean value theorem for integrals applies:

$$R(f) = \int_{-1}^{1} K_1(t)\, f''(t)\,dt = f''(\xi) \int_{-1}^{1} K_1(t)\,dt, \qquad \xi \in (-1,1).$$

Compute the constant via the equivalent shortcut $\int_{-1}^1 K_1\,dt = \dfrac{R(x^{d+1})}{(d+1)!} = \dfrac{R(x^2)}{2!}$, with $R(x^2) = \tfrac25 - \tfrac23\cdot 0 = \tfrac25$:

$$\boxed{\ R(f) = \frac{1}{5}\, f''(\xi), \qquad \xi \in (-1,1)\ }$$

**Sanity check:** for $f = x^2$ ($f'' = 2$): $R = \tfrac15 \cdot 2 = \tfrac25$, matching the direct value $\tfrac25$. ✓

## Two stages, several routes

Finding the **formula** and finding the **remainder** are separate stages, each with more than one valid route — they all give the same result.

**Finding $A_0, x_0$:**
1. *Method of undetermined coefficients* (used above): impose exactness on $1, x$.
2. *Gaussian / orthogonal-polynomial theory*: $x_0$ is the root of the degree-1 polynomial orthogonal to constants under $w$, namely $\pi_1(x) = x - \tfrac{\mu_1}{\mu_0} = x$, root $x_0 = 0$; then $A_0 = \mu_0 = \tfrac23$. Same formula (the Gaussian rule is *the* unique max-precision one).

**Finding $R(f)$:**
1. *Peano kernel theorem* (used above): rigorous; $R(f) = \int K_1(t) f''(t)\,dt$, then MVT since $K_1$ keeps its sign.
2. *"First failed monomial" shortcut*: $R(f) = \dfrac{f^{(d+1)}(\xi)}{(d+1)!}\,R(x^{d+1})$, valid exactly when the kernel keeps its sign — a corollary of Peano.
3. *Direct Taylor expansion*: write $f(x) = f(0) + f'(0)x + \tfrac{f''(\eta_x)}{2}x^2$; then $\int_{-1}^1 x^2 f\,dx = \tfrac23 f(0) + 0 + \tfrac12\int_{-1}^1 x^4 f''(\eta_x)\,dx$, so $R(f) = \tfrac12\int_{-1}^1 x^4 f''(\eta_x)\,dx$. Since $x^4 \ge 0$, MVT gives $R(f) = \tfrac12 f''(\xi)\int_{-1}^1 x^4\,dx = \tfrac12 f''(\xi)\cdot\tfrac25 = \tfrac15 f''(\xi)$.

All three remainder routes yield $R(f) = \tfrac15 f''(\xi)$.

## Key takeaway

For a 1-point weighted rule $\int_a^b w f\,dx = A_0 f(x_0) + R(f)$:
1. Max degree of precision is $1$; fix $A_0, x_0$ from exactness on $1$ and $x$ ($A_0 = \mu_0$, $x_0 = \mu_1/\mu_0$).
2. The remainder uses the first derivative order one above the degree of precision: $R(f) = \dfrac{f^{(d+1)}(\xi)}{(d+1)!}R(x^{d+1})$, provided the Peano kernel does not change sign.
