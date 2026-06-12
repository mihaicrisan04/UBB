# Solution — Numerical Calculus Exam, 24

---

## Problem 1

> (3 points) Use Cholesky factorization to solve the system
> $$\begin{cases} 4x_1 + 2x_2 + 6x_3 = -2 \\ 2x_1 + 2x_2 + 5x_3 = 0 \\ 6x_1 + 5x_2 + 22x_3 = 1 \end{cases}$$

**Formula used — Cholesky:** $A = R^TR$, $R$ upper triangular. Solve $R^Ty = b$, $Rx = y$.

---

$$A = \begin{bmatrix} 4 & 2 & 6 \\ 2 & 2 & 5 \\ 6 & 5 & 22 \end{bmatrix}, \quad b = \begin{bmatrix} -2 \\ 0 \\ 1 \end{bmatrix}$$

**Column 1:** $r_{11} = \sqrt{4} = 2$, $r_{12} = 2/2 = 1$, $r_{13} = 6/2 = 3$

**Column 2:** $r_{22} = \sqrt{2-1^2} = 1$, $r_{23} = (5-1\cdot 3)/1 = 2$

**Column 3:** $r_{33} = \sqrt{22-9-4} = 3$

$$R = \begin{bmatrix} 2 & 1 & 3 \\ 0 & 1 & 2 \\ 0 & 0 & 3 \end{bmatrix}$$

**Solve $R^Ty = b$:**

$$\begin{bmatrix} 2 & 0 & 0 \\ 1 & 1 & 0 \\ 3 & 2 & 3 \end{bmatrix} y = \begin{bmatrix} -2 \\ 0 \\ 1 \end{bmatrix}$$

$2y_1 = -2 \implies y_1 = -1$

$y_1 + y_2 = 0 \implies y_2 = 1$

$3(-1) + 2(1) + 3y_3 = 1 \implies -3+2+3y_3 = 1 \implies 3y_3 = 2 \implies y_3 = 2/3$

**Solve $Rx = y$:**

$3x_3 = 2/3 \implies x_3 = 2/9$

$x_2 + 2(2/9) = 1 \implies x_2 = 1-4/9 = 5/9$

$2x_1 + 5/9 + 3(2/9) = -1 \implies 2x_1 + 5/9 + 6/9 = -1 \implies 2x_1 = -1 - 11/9 = -20/9 \implies x_1 = -10/9$

$$\boxed{x = \begin{bmatrix} -10/9 \\ 5/9 \\ 2/9 \end{bmatrix}}$$

**Verification:**
- $4(-10/9)+2(5/9)+6(2/9) = (-40+10+12)/9 = -18/9 = -2$ $\checkmark$
- $2(-10/9)+2(5/9)+5(2/9) = (-20+10+10)/9 = 0$ $\checkmark$
- $6(-10/9)+5(5/9)+22(2/9) = (-60+25+44)/9 = 9/9 = 1$ $\checkmark$

---

## Problem 2

> (2 points) Define a one-point iterative method to approximate the root $\sqrt{5}$, for a sufficiently close $x_0$. Find the order of convergence and a bound for the error.

**Formula used — Newton's method applied to $f(x) = x^2 - 5 = 0$:**

$$x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)} = x_n - \frac{x_n^2 - 5}{2x_n} = \frac{x_n^2 + 5}{2x_n} = \frac{1}{2}\left(x_n + \frac{5}{x_n}\right)$$

---

$g(x) = \frac{1}{2}\left(x + \frac{5}{x}\right)$, $\alpha = \sqrt{5}$.

$g'(x) = \frac{1}{2}\left(1 - \frac{5}{x^2}\right)$. $g'(\sqrt{5}) = \frac{1}{2}(1-1) = 0$.

$g''(x) = \frac{5}{x^3}$. $g''(\sqrt{5}) = \frac{5}{5\sqrt{5}} = \frac{1}{\sqrt{5}} \neq 0$.

**Order $p = 2$** (quadratic, Newton's method).

Choose $x_0 = 2$ (since $\sqrt{5} \approx 2.236$).

$$\boxed{x_{n+1} = \frac{1}{2}\left(x_n + \frac{5}{x_n}\right), \quad p = 2, \quad |x_{n+1}-\sqrt{5}| \leq \frac{1}{2\sqrt{5}}|x_n-\sqrt{5}|^2}$$

---

## Problem 3

> Let $f \in C^3[0, 1]$.
>
> a) (1 point) Find $(Pf)(x)$, the polynomial interpolating $f$, knowing $f(0)$, $f(1)$ and $f'(0)$.
>
> b) (1 point) Integrate $\int_0^1 (Pf)(x)\,dx$ to obtain a quadrature formula for $\int_0^1 f(x)\,dx$. Find its degree of precision.
>
> c) (1 point) Find $A_0$, $A_1$, $A_2$ such that $\int_0^1 f(x)\,dx = A_0 f(0) + A_1 f'(0) + A_2 f(1) + R(f)$ has max degree of precision. Compare to b).
>
> d) Find the remainder $R(f)$.

### a) Find $(Pf)(x)$

**Formula used — Hermite interpolation:** $x_0 = 0$ double node ($f(0), f'(0)$), $x_1 = 1$ simple ($f(1)$). Degree $\leq 2$.

$P(x) = ax^2 + bx + c$.

$P(0) = f(0) \implies c = f(0)$

$P'(0) = f'(0) \implies b = f'(0)$

$P(1) = f(1) \implies a + b + c = f(1) \implies a = f(1) - f'(0) - f(0)$

$$\boxed{(Pf)(x) = f(0) + f'(0)\,x + (f(1)-f(0)-f'(0))\,x^2}$$

### b) Quadrature formula

$$\int_0^1 (Pf)(x)\,dx = f(0)\cdot 1 + f'(0)\cdot\frac{1}{2} + (f(1)-f(0)-f'(0))\cdot\frac{1}{3}$$

$$= f(0) + \frac{1}{2}f'(0) + \frac{1}{3}f(1) - \frac{1}{3}f(0) - \frac{1}{3}f'(0) = \frac{2}{3}f(0) + \frac{1}{6}f'(0) + \frac{1}{3}f(1)$$

$$\boxed{\int_0^1 f(x)\,dx \approx \frac{2}{3}f(0) + \frac{1}{6}f'(0) + \frac{1}{3}f(1)}$$

**Degree of precision:** $d = 2$ (interpolation is degree 2, so exact for $e_0, e_1, e_2$).

Check $e_3$: $\int_0^1 x^3\,dx = \frac{1}{4}$. $\frac{2}{3}(0)+\frac{1}{6}(0)+\frac{1}{3}(1) = \frac{1}{3} \neq \frac{1}{4}$. $R(e_3) = -\frac{1}{12}$.

$d = 2$.

### c) Undetermined coefficients

3 unknowns ($A_0, A_1, A_2$), max $d = 2$. Set $R(e_k) = 0$ for $k = 0,1,2$:

$k=0$: $1 = A_0 + A_2 \cdot 1 \implies A_0 + A_2 = 1$

$k=1$: $\frac{1}{2} = A_0\cdot 0 + A_1 \cdot 1 + A_2 \cdot 1 \implies A_1 + A_2 = \frac{1}{2}$

$k=2$: $\frac{1}{3} = A_0\cdot 0 + A_1\cdot 0 + A_2\cdot 1 \implies A_2 = \frac{1}{3}$

From $A_2 = 1/3$: $A_0 = 1-1/3 = 2/3$. $A_1 = 1/2-1/3 = 1/6$.

$$\boxed{A_0 = \frac{2}{3}, \quad A_1 = \frac{1}{6}, \quad A_2 = \frac{1}{3}}$$

**Same formula** as in b). $\checkmark$

### d) Remainder

$d = 2$, $R(e_3) = -\frac{1}{12}$.

$$K_2(t) = \frac{1}{2}R_x((x-t)_+^2)$$

$$R_x((x-t)_+^2) = \int_0^1(x-t)_+^2\,dx - \frac{2}{3}(0-t)_+^2 - \frac{1}{6}\left[\frac{\partial}{\partial x}(x-t)_+^2\right]_{x=0} - \frac{1}{3}(1-t)_+^2$$

For $t \in [0,1]$: $(-t)_+^2 = 0$, $(1-t)_+^2 = (1-t)^2$, $\frac{\partial}{\partial x}(x-t)_+^2\big|_{x=0} = 2(0-t)_+ = 0$.

$$\int_0^1(x-t)_+^2\,dx = \int_t^1(x-t)^2\,dx = \frac{(x-t)^3}{3}\Bigg|_t^1 = \frac{(1-t)^3}{3}$$

$$R_x = \frac{(1-t)^3}{3} - \frac{1}{3}(1-t)^2 = \frac{(1-t)^2}{3}[(1-t)-1] = -\frac{t(1-t)^2}{3}$$

$$K_2(t) = -\frac{t(1-t)^2}{6} \leq 0 \quad \text{for } t \in [0,1]$$

Constant sign, Corollary applies:

$$R(f) = \frac{1}{3!}f'''(\xi)\,R(e_3) = \frac{1}{6}\left(-\frac{1}{12}\right)f'''(\xi)$$

$$\boxed{R(f) = -\frac{1}{72}f'''(\xi), \quad \xi \in (0,1)}$$
