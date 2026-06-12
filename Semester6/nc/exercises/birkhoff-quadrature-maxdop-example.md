# Birkhoff Quadrature with Max Degree of Precision + Peano — Worked Example

**Problem.** Let $f \in C^3[0,1]$.

- **a)** Find $(Pf)(x)$, the polynomial interpolating $f$ knowing $f(0)$ and $f'(1/3)$.
- **b)** Integrate $\int_0^1 (Pf)(x)\,dx$ to obtain a quadrature formula for $\int_0^1 f(x)\,dx$. Find its degree of precision.
- **c)** Find $A_0, A_1, x_1$ such that $\int_0^1 f(x)\,dx = A_0 f(0) + A_1 f'(x_1) + R(f)$ has the maximum degree of precision possible. Compare with the formula in b).
- **d)** Find the remainder $R(f)$ using Peano's theorem.

## a) The interpolating polynomial

Two conditions ($f(0)$ and $f'(1/3)$) determine a degree-1 polynomial $P(x) = a + bx$:

$$P(0) = f(0) \;\Rightarrow\; a = f(0), \qquad P'(1/3) = f'(1/3) \;\Rightarrow\; b = f'(1/3) \ \ (\text{since } P'(x) = b).$$

$$\boxed{\,(Pf)(x) = f(0) + x\, f'(1/3)\,}$$

This is a **Birkhoff** interpolation: a function value at $x = 0$ and a derivative value at $x = 1/3$.

## b) Quadrature formula and its degree of precision

$$\int_0^1 (Pf)(x)\,dx = f(0)\underbrace{\int_0^1 dx}_{=1} + f'(1/3)\underbrace{\int_0^1 x\,dx}_{=1/2} = f(0) + \tfrac12 f'(1/3).$$

$$\boxed{\,\int_0^1 f(x)\,dx \approx f(0) + \tfrac12 f'(1/3)\,}$$

**Degree of precision** — test the functional $Q(f) = f(0) + \tfrac12 f'(1/3)$:

| $f$ | $\int_0^1 f\,dx$ | $Q(f)$ | match |
|---|---|---|---|
| $1$ | $1$ | $1 + \tfrac12(0) = 1$ | ✓ |
| $x$ | $\tfrac12$ | $0 + \tfrac12(1) = \tfrac12$ | ✓ |
| $x^2$ | $\tfrac13$ | $0 + \tfrac12\cdot\tfrac23 = \tfrac13$ | ✓ |
| $x^3$ | $\tfrac14$ | $0 + \tfrac12\cdot\tfrac13 = \tfrac16$ | ✗ |

Exact through $x^2$, fails at $x^3$ → **degree of precision $= 2$**. The node $1/3$ is special — it lifts the DoP to 2, one higher than a degree-1 interpolant would normally give.

## c) Maximum-degree-of-precision formula

With three free parameters $A_0, A_1, x_1$, impose exactness on $1, x, x^2$ for $Q(f) = A_0 f(0) + A_1 f'(x_1)$:

- $f = 1$: $\int = 1$, $Q = A_0 \Rightarrow A_0 = 1$.
- $f = x$: $\int = \tfrac12$, $Q = A_1 \Rightarrow A_1 = \tfrac12$.
- $f = x^2$: $\int = \tfrac13$, $Q = A_1\cdot 2x_1 = x_1 \Rightarrow x_1 = \tfrac13$.

$$\boxed{\,A_0 = 1, \quad A_1 = \tfrac12, \quad x_1 = \tfrac13\,}$$

Check $x^3$: $Q = A_1\cdot 3x_1^2 = \tfrac12\cdot\tfrac13 = \tfrac16 \neq \tfrac14$ → fails, so the **max degree of precision is 2**.

**Comparison with b).** The two formulas are **identical**: $\int_0^1 f \approx f(0) + \tfrac12 f'(1/3)$. The node $1/3$ handed to us in part a is exactly the one that maximizes the degree of precision, so integrating the Birkhoff interpolant already produced the optimal (Gaussian-type) rule. With 3 free parameters we can match 3 monomials ($1, x, x^2$), giving DoP 2; a 4th match ($x^3$) is impossible, so 2 is optimal.

## d) Remainder via Peano's theorem

Degree of precision $d = 2$, so the remainder uses $f'''$ (hence $f \in C^3[0,1]$):

$$R(f) = \int_0^1 K(t)\, f'''(t)\,dt, \qquad K(t) = R_x\!\left[\frac{(x-t)_+^2}{2}\right].$$

Apply $R(g) = \int_0^1 g\,dx - g(0) - \tfrac12 g'(1/3)$ to $\dfrac{(x-t)_+^2}{2}$ (with $\tfrac{d}{dx}\tfrac{(x-t)_+^2}{2} = (x-t)_+$):

$$K(t) = \underbrace{\int_t^1 \frac{(x-t)^2}{2}\,dx}_{(1-t)^3/6} - \underbrace{\frac{(0-t)_+^2}{2}}_{=0} - \frac12\,(1/3 - t)_+ = \frac{(1-t)^3}{6} - \frac12\left(\tfrac13 - t\right)_+.$$

Two pieces (the $+$ part switches off at $t = 1/3$):

$$K(t) = \begin{cases} \dfrac{(1-t)^3}{6} - \dfrac12\left(\dfrac13 - t\right), & 0 \le t < \tfrac13 \\[2mm] \dfrac{(1-t)^3}{6}, & \tfrac13 \le t \le 1 \end{cases}$$

**Sign of the kernel.** On $[\tfrac13, 1]$, $K(t) = \tfrac{(1-t)^3}{6} \ge 0$. On $[0, \tfrac13]$, $K'(t) = \tfrac12\big[1 - (1-t)^2\big] > 0$, so $K$ rises from $K(0) = 0$ to $K(\tfrac13) = \tfrac{4}{81} > 0$. Thus $K(t) \ge 0$ on all of $[0,1]$ — **no sign change** — and the mean value theorem applies:

$$R(f) = f'''(\xi)\int_0^1 K(t)\,dt, \qquad \xi \in (0,1).$$

Compute the constant via the shortcut $\int_0^1 K = \dfrac{R(x^3)}{3!}$, with $R(x^3) = \tfrac14 - \tfrac16 = \tfrac{1}{12}$:

$$\int_0^1 K(t)\,dt = \frac{1/12}{6} = \frac{1}{72}.$$

$$\boxed{\,R(f) = \frac{1}{72}\, f'''(\xi), \qquad \xi \in (0,1)\,}$$

**Direct check** of the integral: $\displaystyle\int_0^1 \frac{(1-t)^3}{6}\,dt - \int_0^{1/3}\left(\frac16 - \frac{t}{2}\right)dt = \frac{1}{24} - \frac{1}{36} = \frac{1}{72}$ ✓.

## Key takeaway

- **Interpolate** the gappy value/derivative data (Birkhoff) → degree-1 polynomial.
- **Integrate** it → the quadrature formula; test monomials for the degree of precision.
- **Max-DoP determination**: with $k$ free parameters, impose exactness on $1, x, \dots, x^{k-1}$ and solve. If the interpolation nodes were chosen optimally, this reproduces the same formula (here both give $f(0) + \tfrac12 f'(1/3)$, DoP 2).
- **Peano remainder** at DoP $d$: kernel $K(t) = R_x\big[\tfrac{(x-t)_+^d}{d!}\big]$ (a derivative term $f'(x_0)$ contributes a $(x_0 - t)_+$ piece, making $K$ piecewise). If $K$ keeps its sign, $R(f) = \tfrac{R(x^{d+1})}{(d+1)!} f^{(d+1)}(\xi)$.
