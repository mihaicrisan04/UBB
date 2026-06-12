# 1-Point Weighted Quadrature (Max Degree of Precision) — Example 2

Second example of this type (see `weighted-quadrature-1point-example.md` for the $w(x)=x^2$ version). Here the weight is $w(x) = |x|$.

**Problem.** Let $f \in C^2[-1,1]$. Find a quadrature formula

$$\int_{-1}^{1} |x|\, f(x)\,dx = A\, f(\alpha) + R(f)$$

with the maximum degree of precision possible, and find an expression for the remainder $R(f)$.

## Setup

A 1-point rule has **2 free parameters** ($A, \alpha$), so the maximum achievable degree of precision is $2n - 1 = 1$. Impose exactness on $\{1, x\}$ using the moments of $w(x) = |x|$:

$$\mu_0 = \int_{-1}^{1} |x|\,dx = 2\int_0^1 x\,dx = 1, \qquad \mu_1 = \int_{-1}^{1} |x|\,x\,dx = 0 \ (\text{odd integrand}).$$

- $f = 1$: $\quad A = \mu_0 = 1.$
- $f = x$: $\quad A\alpha = \mu_1 = 0 \ \Rightarrow\ \alpha = 0$ (since $A \neq 0$).

## The formula

$$\boxed{\ \int_{-1}^{1} |x|\, f(x)\,dx = f(0) + R(f)\ }$$

**Degree of precision check:** exact for $1$ ($1 = 1$) and $x$ ($0 = 0$). For $x^2$: LHS $= \mu_2 = \int_{-1}^{1} |x|\,x^2\,dx = 2\int_0^1 x^3\,dx = \tfrac12$, but RHS $= f(0) = 0$. Fails at degree 2 → **degree of precision $= 1$**.

## Remainder for $f \in C^2[-1,1]$ (Peano)

The error functional $R(g) = \int_{-1}^{1} |x|\,g(x)\,dx - g(0)$ has degree of precision $d = 1$, so the Peano kernel is $K(t) = R_x[(x-t)_+]$. Computing it (splitting the $|x|$ at $0$) gives the **even** kernel

$$K(t) = \frac13 - \frac{|t|}{2} + \frac{|t|^3}{6}.$$

**Sign of the kernel.** With $s = |t| \in [0,1]$, $h(s) = \tfrac13 - \tfrac{s}{2} + \tfrac{s^3}{6}$ has $h'(s) = \tfrac{s^2 - 1}{2} \le 0$, so $h$ decreases from $h(0) = \tfrac13$ to $h(1) = 0$. Thus $K(t) \ge 0$ on $[-1,1]$ — **no sign change** — and the mean value theorem applies:

$$R(f) = f''(\xi) \int_{-1}^{1} K(t)\,dt, \qquad \xi \in (-1,1).$$

Compute the constant via the shortcut $\int_{-1}^1 K\,dt = \dfrac{R(x^2)}{2!}$, with $R(x^2) = \mu_2 - 0 = \tfrac12$:

$$\boxed{\ R(f) = \frac14\, f''(\xi), \qquad \xi \in (-1,1)\ }$$

**Checks.**
- Direct integral: $\int_{-1}^{1} K\,dt = 2\int_0^1\left(\tfrac13 - \tfrac{t}{2} + \tfrac{t^3}{6}\right)dt = 2\cdot\tfrac18 = \tfrac14$. ✓
- $f = x^2$ ($f'' = 2$): $R = \tfrac14 \cdot 2 = \tfrac12$, matching the direct value $\tfrac12$. ✓

## Key takeaway

Identical procedure to the $w(x)=x^2$ case — only the moments change with the weight:
1. Max degree of precision $= 1$; fix $A = \mu_0$, $\alpha = \mu_1/\mu_0$ (here $\mu_1 = 0 \Rightarrow \alpha = 0$).
2. Remainder $R(f) = \dfrac{R(x^{d+1})}{(d+1)!}\,f^{(d+1)}(\xi) = \dfrac{\mu_2}{2}\,f''(\xi)$ once the Peano kernel is shown to keep its sign. The weight $|x|$ gives $\tfrac14 f''(\xi)$; the weight $x^2$ gave $\tfrac15 f''(\xi)$.
