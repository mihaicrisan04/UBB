# Solution — Numerical Calculus Exam, 8

---

## Problem 1

> Let $f \in C^3[0, 1]$.
>
> a) (2 points) Find $(Pf)(x)$, the polynomial interpolating the function $f$ at the double node $x_0 = 0$ and the simple node $x_1 = 1$.
>
> b) (1 point) Integrate $\int_{0}^{1}(Pf)(x)\,dx$ to obtain a quadrature formula for $\int_{0}^{1}f(x)\,dx$. Find its degree of precision.
>
> c) (1.5 points) Use Peano's theorem to find the remainder $R(f)$ of the quadrature formula in b).

### a) Find $(Pf)(x)$

**Formula used — Hermite interpolation (mixed nodes):** Double node at $x_0 = 0$ means we use $f(0)$ and $f'(0)$. Simple node at $x_1 = 1$ means we use $f(1)$. Total conditions: 3, so degree $\leq 2$.

$$P(x) = ax^2 + bx + c$$

Conditions:
$$P(0) = f(0) \implies c = f(0)$$
$$P'(0) = f'(0) \implies b = f'(0)$$
$$P(1) = f(1) \implies a + b + c = f(1) \implies a = f(1) - f'(0) - f(0)$$

**Using Newton's divided differences for mixed nodes:**

Nodes: $z_0 = 0, z_1 = 0, z_2 = 1$ (double at 0, simple at 1).

Divided differences table:

| $z_i$ | $f[z_i]$ | 1st | 2nd |
|--------|-----------|-----|-----|
| $0$ | $f(0)$ | $f'(0)$ | $\frac{f[0,1] - f'(0)}{1-0}$ |
| $0$ | $f(0)$ | $f[0,1] = \frac{f(1)-f(0)}{1}$ | |
| $1$ | $f(1)$ | | |

$$f[0,0] = f'(0), \quad f[0,1] = f(1) - f(0)$$

$$f[0,0,1] = \frac{f[0,1] - f[0,0]}{1 - 0} = \frac{(f(1)-f(0)) - f'(0)}{1} = f(1) - f(0) - f'(0)$$

Newton form:

$$(Pf)(x) = f(0) + f'(0)(x-0) + (f(1)-f(0)-f'(0))(x-0)^2$$

$$\boxed{(Pf)(x) = f(0) + f'(0)\,x + \big(f(1) - f(0) - f'(0)\big)\,x^2}$$

### b) Quadrature formula and degree of precision

$$\int_0^1 (Pf)(x)\,dx = f(0)\int_0^1 1\,dx + f'(0)\int_0^1 x\,dx + (f(1)-f(0)-f'(0))\int_0^1 x^2\,dx$$

**Computing each integral:**

$\int_0^1 1\,dx = x\Big|_0^1 = 1$

$\int_0^1 x\,dx = \frac{x^2}{2}\Big|_0^1 = \frac{1}{2} - 0 = \frac{1}{2}$

$\int_0^1 x^2\,dx = \frac{x^3}{3}\Big|_0^1 = \frac{1}{3} - 0 = \frac{1}{3}$

$$= f(0) \cdot 1 + f'(0) \cdot \frac{1}{2} + (f(1)-f(0)-f'(0)) \cdot \frac{1}{3}$$

$$= f(0) + \frac{1}{2}f'(0) + \frac{1}{3}f(1) - \frac{1}{3}f(0) - \frac{1}{3}f'(0)$$

$$= \frac{2}{3}f(0) + \frac{1}{6}f'(0) + \frac{1}{3}f(1)$$

$$\boxed{\int_0^1 f(x)\,dx \approx \frac{2}{3}f(0) + \frac{1}{6}f'(0) + \frac{1}{3}f(1)}$$

**Degree of precision:** $R(f) = \int_0^1 f(x)\,dx - \frac{2}{3}f(0) - \frac{1}{6}f'(0) - \frac{1}{3}f(1)$

Since the interpolation uses 3 data points (degree 2 polynomial), $R(e_k) = 0$ for $k = 0, 1, 2$ automatically. Check $e_3(x) = x^3$:

$$\int_0^1 x^3\,dx = \frac{x^4}{4}\Bigg|_0^1 = \frac{1}{4}$$

$f(0) = 0$, $f'(0) = 0$ (since $(x^3)' = 3x^2$, at $x=0$: $0$), $f(1) = 1$.

$$R(e_3) = \frac{1}{4} - \frac{2}{3}(0) - \frac{1}{6}(0) - \frac{1}{3}(1) = \frac{1}{4} - \frac{1}{3} = \frac{3-4}{12} = -\frac{1}{12} \neq 0$$

Degree of precision $d = 2$.

### c) Remainder using Peano's theorem

**Formula used:** $d = 2 \implies R(f) = \int_0^1 K_2(t)\,f'''(t)\,dt$ with $K_2(t) = \frac{1}{2!}R_x((x-t)_+^2)$.

**Corollary:** If $K_2$ constant sign, $R(f) = \frac{1}{3!}f'''(\xi)\,R(e_3)$.

---

$$K_2(t) = \frac{1}{2}R_x\big((x-t)_+^2\big)$$

$$R_x\big((x-t)_+^2\big) = \int_0^1 (x-t)_+^2\,dx - \frac{2}{3}(0-t)_+^2 - \frac{1}{6}\left[\frac{\partial}{\partial x}(x-t)_+^2\right]_{x=0} - \frac{1}{3}(1-t)_+^2$$

For $t \in [0,1]$:
- $(0-t)_+^2 = (-t)_+^2 = 0$ (since $t \geq 0$)
- $\frac{\partial}{\partial x}(x-t)_+^2 = 2(x-t)_+$. At $x=0$: $2(0-t)_+ = 2(-t)_+ = 0$.
- $(1-t)_+^2 = (1-t)^2$

$$\int_0^1 (x-t)_+^2\,dx = \int_t^1 (x-t)^2\,dx$$

Antiderivative of $(x-t)^2$: $\frac{(x-t)^3}{3}$

$$= \frac{(x-t)^3}{3}\Bigg|_t^1 = \frac{(1-t)^3}{3} - 0 = \frac{(1-t)^3}{3}$$

$$R_x\big((x-t)_+^2\big) = \frac{(1-t)^3}{3} - 0 - 0 - \frac{1}{3}(1-t)^2 = \frac{(1-t)^2}{3}\big[(1-t) - 1\big] = \frac{(1-t)^2(-t)}{3}$$

$$K_2(t) = \frac{1}{2}\cdot\frac{-t(1-t)^2}{3} = -\frac{t(1-t)^2}{6} \leq 0 \quad \text{for } t \in [0,1]$$

Constant sign ($\leq 0$), so Corollary applies:

$$R(f) = \frac{1}{3!}f'''(\xi)\,R(e_3) = \frac{1}{6}\cdot f'''(\xi)\cdot\left(-\frac{1}{12}\right)$$

$$\boxed{R(f) = -\frac{1}{72}f'''(\xi), \quad \xi \in (0,1)}$$

---

## Problem 2

> (2.5 points) Find the Cholesky decomposition of the Hilbert matrix
> $$H_3 = \begin{bmatrix} 1 & 1/2 & 1/3 \\ 1/2 & 1/3 & 1/4 \\ 1/3 & 1/4 & 1/5 \end{bmatrix}$$

**Formula used — Cholesky factorization:** For a symmetric positive definite matrix $A$, find upper triangular $R$ with $r_{ii} > 0$ such that $A = R^T R$.

Compute $R$ column by column:

$$r_{ij} = \frac{1}{r_{jj}}\left(a_{ij} - \sum_{k=1}^{j-1} r_{ki}\,r_{kj}\right), \quad i \leq j$$
$$r_{jj} = \sqrt{a_{jj} - \sum_{k=1}^{j-1} r_{kj}^2}$$

---

**Column 1:**

$$r_{11} = \sqrt{a_{11}} = \sqrt{1} = 1$$
$$r_{12} = \frac{a_{12}}{r_{11}} = \frac{1/2}{1} = \frac{1}{2}$$
$$r_{13} = \frac{a_{13}}{r_{11}} = \frac{1/3}{1} = \frac{1}{3}$$

**Column 2:**

$$r_{22} = \sqrt{a_{22} - r_{12}^2} = \sqrt{\frac{1}{3} - \frac{1}{4}} = \sqrt{\frac{4-3}{12}} = \sqrt{\frac{1}{12}} = \frac{1}{2\sqrt{3}}$$

$$r_{23} = \frac{a_{23} - r_{12}\,r_{13}}{r_{22}} = \frac{\frac{1}{4} - \frac{1}{2}\cdot\frac{1}{3}}{\frac{1}{2\sqrt{3}}} = \frac{\frac{1}{4} - \frac{1}{6}}{\frac{1}{2\sqrt{3}}} = \frac{\frac{3-2}{12}}{\frac{1}{2\sqrt{3}}} = \frac{\frac{1}{12}}{\frac{1}{2\sqrt{3}}} = \frac{1}{12}\cdot 2\sqrt{3} = \frac{\sqrt{3}}{6}$$

**Column 3:**

$$r_{33} = \sqrt{a_{33} - r_{13}^2 - r_{23}^2} = \sqrt{\frac{1}{5} - \frac{1}{9} - \frac{3}{36}} = \sqrt{\frac{1}{5} - \frac{1}{9} - \frac{1}{12}}$$

Common denominator (180):

$$= \sqrt{\frac{36}{180} - \frac{20}{180} - \frac{15}{180}} = \sqrt{\frac{1}{180}} = \frac{1}{\sqrt{180}} = \frac{1}{6\sqrt{5}}$$

**Result:**

$$\boxed{R = \begin{bmatrix} 1 & 1/2 & 1/3 \\ 0 & \frac{1}{2\sqrt{3}} & \frac{\sqrt{3}}{6} \\ 0 & 0 & \frac{1}{6\sqrt{5}} \end{bmatrix}, \quad H_3 = R^T R}$$

---

## Problem 3

> (2 points) Show that the polynomial $3x^4 - 8x^3 + 6x^2 - x$ has $\alpha = 1$ as a root. Does the iteration $x_{n+1} = 3x_n^4 - 8x_n^3 + 6x_n^2$ converge to $\alpha$ for a suitable $x_0$? Study the convergence of the method, find the order of convergence and give a bound for the error.

**Formula used — one-point iteration $x_{n+1} = g(x_n)$:** Same as Problems 1/2 of other exams.

---

Let $p(x) = 3x^4 - 8x^3 + 6x^2 - x$.

**Show $\alpha = 1$ is a root:**

$$p(1) = 3 - 8 + 6 - 1 = 0 \checkmark$$

**Iteration:** $g(x) = 3x^4 - 8x^3 + 6x^2 = p(x) + x$.

**Check fixed point:** $g(1) = 3 - 8 + 6 = 1$ $\checkmark$

**Compute derivatives:**

$$g'(x) = 12x^3 - 24x^2 + 12x \implies g'(1) = 12 - 24 + 12 = 0$$

$$g''(x) = 36x^2 - 48x + 12 \implies g''(1) = 36 - 48 + 12 = 0$$

$$g'''(x) = 72x - 48 \implies g'''(1) = 72 - 48 = 24 \neq 0$$

Since $g'(1) = g''(1) = 0$ and $g'''(1) = 24 \neq 0$:

- The method **converges** for $x_0$ sufficiently close to $1$ (since $|g'(1)| = 0 < 1$).
- **Order of convergence $p = 3$.**

**Error bound:**

$$\boxed{|x_{n+1} - 1| \approx \frac{|g'''(1)|}{3!}|x_n - 1|^3 = \frac{24}{6}|x_n - 1|^3 = 4|x_n - 1|^3}$$
