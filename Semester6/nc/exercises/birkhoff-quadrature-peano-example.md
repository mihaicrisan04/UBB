# Birkhoff Interpolation → Quadrature → Peano Remainder — Worked Example

**Problem.** Let $f \in C^2[-1,1]$.

- **a)** Find $(Pf)(x)$, the polynomial interpolating $f$ given $f(-1)$ and $f'(1)$.
- **b)** Integrate $\int_{-1}^{1}(Pf)(x)\,dx$ to obtain a quadrature formula for $\int_{-1}^{1} f(x)\,dx$. Find its degree of precision.
- **c)** Find the remainder $R(f)$ of that quadrature formula using Peano's theorem.

## a) The interpolating polynomial

Two conditions ($f(-1)$ and $f'(1)$) determine a polynomial of degree $\le 1$. Write $P(x) = a + bx$ and impose:

$$P(-1) = f(-1), \qquad P'(1) = f'(1).$$

Since $P'(x) = b$, the second condition gives $b = f'(1)$. The first gives $a - b = f(-1)$, so $a = f(-1) + f'(1)$. Hence

$$\boxed{\,(Pf)(x) = f(-1) + (x+1)\,f'(1)\,}$$

**Check:** $P(-1) = f(-1) + (-1+1)f'(1) = f(-1)$ ✓, and $P'(x) = f'(1) \Rightarrow P'(1) = f'(1)$ ✓.

This is a **Birkhoff** interpolation: a function value at $x = -1$ and a derivative value at $x = 1$, with a "gap" (no function value at $x=1$, no derivative at $x=-1$). The "B" method here is *Birkhoff* — not Cholesky, which is an unrelated matrix factorization ($A=R^TR$) for solving linear systems, nothing to do with interpolation.

Equivalently, via fundamental polynomials: $A_0(x) = 1$ (with $A_0(-1)=1$, $A_0'(1)=0$) and $A_1(x) = x+1$ (with $A_1(-1)=0$, $A_1'(1)=1$), so $(Pf)(x) = f(-1)A_0(x) + f'(1)A_1(x)$.

## b) The quadrature formula and its degree of precision

$$\int_{-1}^{1}(Pf)(x)\,dx = f(-1)\underbrace{\int_{-1}^{1}dx}_{=\,2} + f'(1)\underbrace{\int_{-1}^{1}(x+1)\,dx}_{=\,2},$$

where $\int_{-1}^{1}(x+1)\,dx = \left[\tfrac{x^2}{2}+x\right]_{-1}^{1} = \tfrac32 - (-\tfrac12) = 2$. Therefore

$$\boxed{\,\int_{-1}^{1} f(x)\,dx \approx 2f(-1) + 2f'(1)\,}$$

**Degree of precision** — test monomials against the functional $Q(f) = 2f(-1) + 2f'(1)$:

| $f$ | $\int_{-1}^{1} f\,dx$ | $Q(f) = 2f(-1)+2f'(1)$ | match |
|---|---|---|---|
| $1$ | $2$ | $2(1) + 2(0) = 2$ | ✓ |
| $x$ | $0$ | $2(-1) + 2(1) = 0$ | ✓ |
| $x^2$ | $\tfrac23$ | $2(1) + 2(2) = 6$ | ✗ |

Exact through degree 1, fails at degree 2 → **degree of precision $= 1$**.

## c) Remainder via Peano's theorem

The error functional

$$R(f) = \int_{-1}^{1} f(x)\,dx - 2f(-1) - 2f'(1)$$

vanishes on $\mathbb{P}_1$ (degree of precision $d = 1$). By Peano's theorem, for $f \in C^2[-1,1]$,

$$R(f) = \int_{-1}^{1} K(t)\, f''(t)\,dt, \qquad K(t) = R_x\big[(x-t)_+\big],$$

where $R_x$ applies the functional in the variable $x$, and $\tfrac{d}{dx}(x-t)_+ = H(x-t)$ (the unit step).

For $t \in (-1,1)$, term by term:

$$K(t) = \underbrace{\int_{t}^{1}(x-t)\,dx}_{\frac{(1-t)^2}{2}} \;-\; \underbrace{2\,(-1-t)_+}_{=\,0\ (\text{since } -1-t<0)} \;-\; \underbrace{2\,H(1-t)}_{=\,2\ (\text{since } 1-t>0)} = \frac{(1-t)^2}{2} - 2.$$

**Sign of the kernel.** $K(t) = \tfrac{(1-t)^2}{2} - 2$ has its only root on $[-1,1]$ at $t=-1$ (where $K=0$); it decreases to $K(1) = -2$. So $K(t) \le 0$ throughout $[-1,1]$ — **it does not change sign.** Hence the mean value theorem for integrals applies:

$$R(f) = f''(\xi)\int_{-1}^{1} K(t)\,dt, \qquad \xi \in (-1,1).$$

Compute the integral:

$$\int_{-1}^{1}\left[\frac{(1-t)^2}{2} - 2\right]dt = \frac12\int_{-1}^{1}(1-t)^2\,dt - 4 = \frac12\cdot\frac{8}{3} - 4 = \frac{4}{3} - 4 = -\frac{8}{3}.$$

$$\boxed{\,R(f) = -\frac{8}{3}\, f''(\xi), \qquad \xi \in (-1,1)\,}$$

**Cross-check** with the shortcut $R(f) = \dfrac{f''(\xi)}{2!}\,R(x^2)$: here $R(x^2) = \tfrac23 - 2(1) - 2(2) = -\tfrac{16}{3}$, so $\tfrac12\left(-\tfrac{16}{3}\right) = -\tfrac83$ ✓.

## Key takeaway

1. **Interpolate** the given data (value + derivative at different nodes = Birkhoff) → a degree-1 polynomial.
2. **Integrate** the interpolant to read off the quadrature weights; its degree of precision is found by testing monomials (here $d=1$).
3. **Peano remainder**: build the kernel $K(t) = R_x[(x-t)_+^{\,d}/d!]$ (here $d=1$); if $K$ keeps its sign, pull out $f^{(d+1)}(\xi)$ and the constant is $\int K = \tfrac{R(x^{d+1})}{(d+1)!}$. The functional must be applied **including its derivative terms** — that is why a $-2H(1-t)$ piece appears from the $f'(1)$ term.
