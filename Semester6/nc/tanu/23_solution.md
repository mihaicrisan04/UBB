# Solution — Numerical Calculus Exam, 23

---

## Problem 1

> Let $f: [0, 1] \to \mathbb{R}$ be three times differentiable.
>
> a) (1 point) Find $a$, $b$, $c$ such that $\int_0^1 f(x)\,dx = a\,f(0) + b\,f(1) + c\,f'(0) + R(f)$ has the maximum degree of precision possible. What is that degree?
>
> b) (1 point) Find $(Pf)(x)$, the polynomial interpolating $f$, given $f(0)$, $f(1)$ and $f'(0)$.
>
> c) (1 point) Integrate $\int_0^1 (Pf)(x)\,dx$ and compare with part a).
>
> d) (1 point) Find the remainder $R(f)$.

### a) Undetermined coefficients

**Formula used:** 3 unknowns ($a,b,c$), so max $d = 2$. Set $R(e_k) = 0$ for $k = 0,1,2$.

**$k=0$: $f=1$, $f(0)=1$, $f(1)=1$, $f'(0)=0$.**

$\int_0^1 1\,dx = 1$. So $a + b = 1$.

**$k=1$: $f=x$, $f(0)=0$, $f(1)=1$, $f'(0)=1$.**

$\int_0^1 x\,dx = \frac{x^2}{2}\Big|_0^1 = \frac{1}{2}$. So $b + c = \frac{1}{2}$.

**$k=2$: $f=x^2$, $f(0)=0$, $f(1)=1$, $f'(x)=2x$, $f'(0)=0$.**

$\int_0^1 x^2\,dx = \frac{x^3}{3}\Big|_0^1 = \frac{1}{3}$. So $b = \frac{1}{3}$.

From $b = 1/3$: $a = 1 - 1/3 = 2/3$. $c = 1/2 - 1/3 = 1/6$.

$$\boxed{a = \frac{2}{3}, \quad b = \frac{1}{3}, \quad c = \frac{1}{6}}$$

**Check $k=3$:** $f=x^3$, $f(0)=0$, $f(1)=1$, $f'(0)=0$.

$\int_0^1 x^3\,dx = \frac{1}{4}$. Quadrature: $\frac{2}{3}(0)+\frac{1}{3}(1)+\frac{1}{6}(0) = \frac{1}{3} \neq \frac{1}{4}$.

So $R(e_3) = \frac{1}{4} - \frac{1}{3} = -\frac{1}{12} \neq 0$. Degree of precision $d = 2$.

$$\boxed{\int_0^1 f(x)\,dx = \frac{2}{3}f(0) + \frac{1}{3}f(1) + \frac{1}{6}f'(0) + R(f), \quad d = 2}$$

### b) Find $(Pf)(x)$

**Formula used — Hermite interpolation:** $x_0 = 0$ (double: $f(0), f'(0)$), $x_1 = 1$ (simple: $f(1)$). Degree $\leq 2$, so $P(x) = Ax^2 + Bx + C$.

$$P(0) = f(0) \implies C = f(0)$$
$$P'(0) = f'(0) \implies B = f'(0)$$
$$P(1) = f(1) \implies A + B + C = f(1) \implies A = f(1) - f'(0) - f(0)$$

$$\boxed{(Pf)(x) = f(0) + f'(0)\,x + (f(1)-f(0)-f'(0))\,x^2}$$

### c) Integrate and compare

$$\int_0^1 (Pf)(x)\,dx = f(0)\int_0^1 1\,dx + f'(0)\int_0^1 x\,dx + (f(1)-f(0)-f'(0))\int_0^1 x^2\,dx$$

$\int_0^1 1\,dx = 1$, $\int_0^1 x\,dx = \frac{1}{2}$, $\int_0^1 x^2\,dx = \frac{1}{3}$.

$$= f(0) + \frac{1}{2}f'(0) + \frac{1}{3}(f(1)-f(0)-f'(0))$$

$$= f(0) + \frac{1}{2}f'(0) + \frac{1}{3}f(1) - \frac{1}{3}f(0) - \frac{1}{3}f'(0)$$

$$= \frac{2}{3}f(0) + \frac{1}{6}f'(0) + \frac{1}{3}f(1)$$

This is **the same formula** as in part a). $\checkmark$

### d) Remainder

$d = 2$, $R(e_3) = -\frac{1}{12}$.

$K_2(t) = \frac{1}{2}R_x((x-t)_+^2)$. We need to check constant sign.

$$R_x((x-t)_+^2) = \int_0^1(x-t)_+^2\,dx - \frac{2}{3}(0-t)_+^2 - \frac{1}{3}(1-t)_+^2 - \frac{1}{6}\left[\frac{\partial}{\partial x}(x-t)_+^2\right]_{x=0}$$

For $t \in [0,1]$: $(-t)_+^2 = 0$, $(1-t)_+^2 = (1-t)^2$, $\frac{\partial}{\partial x}(x-t)_+^2|_{x=0} = 2(0-t)_+ = 0$.

$\int_0^1(x-t)_+^2\,dx = \frac{(1-t)^3}{3}$.

$$R_x = \frac{(1-t)^3}{3} - \frac{1}{3}(1-t)^2 = \frac{(1-t)^2}{3}[(1-t)-1] = -\frac{t(1-t)^2}{3}$$

$$K_2(t) = -\frac{t(1-t)^2}{6} \leq 0 \quad \text{for } t \in [0,1]$$

Constant sign. Corollary applies:

$$R(f) = \frac{1}{3!}f'''(\xi)\,R(e_3) = \frac{1}{6}\cdot\left(-\frac{1}{12}\right)\cdot f'''(\xi)$$

$$\boxed{R(f) = -\frac{1}{72}f'''(\xi), \quad \xi \in (0,1)}$$

---

## Problem 2

> (3 points) Let $A = \begin{bmatrix} 4 & 2 & 6 \\ 2 & 2 & 5 \\ 6 & 5 & 22 \end{bmatrix}$. Use Cholesky factorization to solve $Ax = b$, where $b = [-2\ 0\ 1]^T$.

**Formula used — Cholesky:** $A = R^TR$ where $R$ is upper triangular with $r_{ii} > 0$. Solve $R^Ty = b$, then $Rx = y$.

---

**Column 1:**

$r_{11} = \sqrt{4} = 2$, $r_{12} = \frac{2}{2} = 1$, $r_{13} = \frac{6}{2} = 3$

**Column 2:**

$r_{22} = \sqrt{2 - 1^2} = \sqrt{1} = 1$

$r_{23} = \frac{5 - 1\cdot 3}{1} = 2$

**Column 3:**

$r_{33} = \sqrt{22 - 3^2 - 2^2} = \sqrt{22-9-4} = \sqrt{9} = 3$

$$R = \begin{bmatrix} 2 & 1 & 3 \\ 0 & 1 & 2 \\ 0 & 0 & 3 \end{bmatrix}$$

**Solve $R^T y = b$ (forward sub):**

$$R^T = \begin{bmatrix} 2 & 0 & 0 \\ 1 & 1 & 0 \\ 3 & 2 & 3 \end{bmatrix}$$

$$2y_1 = -2 \implies y_1 = -1$$
$$y_1 + y_2 = 0 \implies y_2 = 0-(-1) = 1$$
$$3y_1 + 2y_2 + 3y_3 = 1 \implies -3+2+3y_3 = 1 \implies 3y_3 = 2 \implies y_3 = \frac{2}{3}$$

**Solve $Rx = y$ (backward sub):**

$$3x_3 = 2/3 \implies x_3 = \frac{2}{9}$$
$$x_2 + 2\cdot\frac{2}{9} = 1 \implies x_2 = 1-\frac{4}{9} = \frac{5}{9}$$
$$2x_1 + \frac{5}{9} + 3\cdot\frac{2}{9} = -1 \implies 2x_1 + \frac{5}{9}+\frac{6}{9} = -1 \implies 2x_1 = -1-\frac{11}{9} = -\frac{20}{9} \implies x_1 = -\frac{10}{9}$$

$$\boxed{x = \begin{bmatrix} -10/9 \\ 5/9 \\ 2/9 \end{bmatrix}}$$

**Verification:** $Ax = [4(-10/9)+2(5/9)+6(2/9),\ \ldots]^T = [(-40+10+12)/9, \ldots]^T = [-18/9, \ldots]^T = [-2, 0, 1]^T$ $\checkmark$

---

## Problem 3

> (2 points) Derive a one-point iteration method that approximates the root $\sqrt{2}$, for a suitable initial value $x_0$. Discuss the order of convergence and the bound for the error.

**Formula used — fixed-point iteration from $f(x) = 0$:** Rewrite $x^2 - 2 = 0$ as $x = g(x)$ and iterate $x_{n+1} = g(x_n)$.

---

$f(x) = x^2 - 2 = 0$, $\alpha = \sqrt{2}$.

**Method 1 (Newton's method):** $g(x) = x - \frac{f(x)}{f'(x)} = x - \frac{x^2-2}{2x} = \frac{x^2+2}{2x} = \frac{1}{2}\left(x + \frac{2}{x}\right)$

$$x_{n+1} = \frac{1}{2}\left(x_n + \frac{2}{x_n}\right)$$

$g'(x) = \frac{1}{2}\left(1 - \frac{2}{x^2}\right)$. $g'(\sqrt{2}) = \frac{1}{2}(1-1) = 0$.

$g''(x) = \frac{2}{x^3}$. $g''(\sqrt{2}) = \frac{2}{2\sqrt{2}} = \frac{1}{\sqrt{2}} \neq 0$.

**Order $p = 2$** (quadratic convergence, since it's Newton's method).

**Error:** $|x_{n+1} - \sqrt{2}| \approx \frac{|g''(\sqrt{2})|}{2}|x_n-\sqrt{2}|^2 = \frac{1}{2\sqrt{2}}|x_n-\sqrt{2}|^2$

Choose $x_0 = 1$ (or any positive value).

$$\boxed{x_{n+1} = \frac{1}{2}\left(x_n + \frac{2}{x_n}\right), \quad p = 2, \quad |x_{n+1}-\sqrt{2}| \leq \frac{1}{2\sqrt{2}}|x_n-\sqrt{2}|^2}$$
