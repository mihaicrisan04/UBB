# Solution — Numerical Calculus Exam, 17

---

## Problem 1

> Consider the iteration $x_{n+1} = 2 - (1+c)x_n + cx_n^2$.
> $\alpha = 1$, order of convergence, error bound.

**Formula used — one-point iteration $x_{n+1} = g(x_n)$:** Convergence iff $|g'(\alpha)| < 1$. Order determined by first nonzero derivative at $\alpha$.

---

$g(x) = 2 - (1+c)x + cx^2$. $g(1) = 2 - 1 - c + c = 1$ $\checkmark$

$g'(x) = -(1+c) + 2cx$. $g'(1) = c - 1$.

$|c-1| < 1 \iff \boxed{0 < c < 2}$

For $c \neq 1$: **order $p = 1$**, rate $|c-1|$, bound: $|x_n - 1| \leq |c-1|^n|x_0-1|$.

For $c = 1$: $g'(1)=0$, $g''(x)=2c=2\neq 0$, **order $p = 2$**, bound: $|x_{n+1}-1| \leq |x_n-1|^2$.

---

## Problem 2

> $f: [-1, 1] \to \mathbb{R}$ twice continuously differentiable. Derive a 1-point numerical integration formula:
> $$\int_{-1}^{1} |x|\,f(x)\,dx = A\,f(\alpha) + R(f)$$
> with max degree of exactness possible. Express the remainder $R(f)$ in terms of $f$.

**Formula used — method of undetermined coefficients:** 2 unknowns, max $d = 1$.

---

**$k=0$: $f = 1$**

$\int_{-1}^{1}|x|\,dx = 2\int_0^1 x\,dx = 2\cdot\frac{x^2}{2}\Big|_0^1 = 2\cdot\frac{1}{2} = 1$. So $A = 1$.

**$k=1$: $f = x$**

$\int_{-1}^{1}|x|\cdot x\,dx = 0$ (odd function). So $A\alpha = 0 \implies \alpha = 0$.

**Check $k=2$:** $\int_{-1}^{1}|x|x^2\,dx = 2\int_0^1 x^3\,dx = 2\cdot\frac{1}{4} = \frac{1}{2}$. $A\alpha^2 = 0 \neq \frac{1}{2}$.

$d = 1$.

$$\boxed{\int_{-1}^{1}|x|f(x)\,dx = f(0) + R(f)}$$

$R(e_2) = \frac{1}{2}$. Remainder: $R(f) = \frac{1}{2!}f''(\xi)\cdot\frac{1}{2} = \frac{1}{4}f''(\xi)$, $\xi \in (-1,1)$.

---

## Problem 3

> $A = \begin{bmatrix} 2 & 1 \\ 1 & 2 \end{bmatrix}$, $b = \begin{bmatrix} -1 \\ -2 \end{bmatrix}$, $Ax = b$. Gauss-Seidel, $x^{(0)} = [0\ 0]^T$.

**Formula used — Gauss-Seidel:** $x_i^{(k+1)} = \frac{1}{a_{ii}}\big(b_i - \sum_{j<i}a_{ij}x_j^{(k+1)} - \sum_{j>i}a_{ij}x_j^{(k)}\big)$

---

$$x_1^{(k+1)} = \frac{1}{2}(-1 - x_2^{(k)}), \quad x_2^{(k+1)} = \frac{1}{2}(-2 - x_1^{(k+1)})$$

**Iteration 1:** $x_1^{(1)} = \frac{1}{2}(-1) = -\frac{1}{2}$, $x_2^{(1)} = \frac{1}{2}(-2+\frac{1}{2}) = -\frac{3}{4}$

**Iteration 2:** $x_1^{(2)} = \frac{1}{2}(-1+\frac{3}{4}) = -\frac{1}{8}$, $x_2^{(2)} = \frac{1}{2}(-2+\frac{1}{8}) = -\frac{15}{16}$

**Iteration 3:** $x_1^{(3)} = \frac{1}{2}(-1+\frac{15}{16}) = -\frac{1}{32}$, $x_2^{(3)} = \frac{1}{2}(-2+\frac{1}{32}) = -\frac{63}{64}$

$$\boxed{x^{(3)} = [-1/32,\ -63/64]^T}$$

*Exact: $x = [0, -1]^T$.*

---

## Problem 4

> Consider the data $(0, 1)$, $(1, -3)$, $(2, 7)$. Find the best linear polynomial.

**Formula used — discrete least squares normal equations.**

---

$N=3$. $\sum x_i = 3$, $\sum x_i^2 = 5$, $\sum y_i = 5$, $\sum x_iy_i = 0+(-3)+14 = 11$.

$$\begin{cases} 3a + 3b = 5 \\ 3a + 5b = 11 \end{cases}$$

$2b = 6 \implies b = 3$. $3a = 5-9 = -4 \implies a = -4/3$.

$$\boxed{\varphi(x) = -\frac{4}{3} + 3x}$$
