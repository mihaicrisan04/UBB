# Solution — Numerical Calculus Exam, 16

---

## Problem 1

> (2.5 points) Find the linear least squares polynomial approximation that best fits the following data:
>
> | $x_i$ | 0 | 1 | 2 |
> |--------|---|---|---|
> | $y_i$ | 1 | $-3$ | 7 |

**Formula used — discrete least squares:** Fit $\varphi(x) = a + bx$. Normal equations:

$$\begin{cases} Na + (\sum x_i)b = \sum y_i \\ (\sum x_i)a + (\sum x_i^2)b = \sum x_i y_i \end{cases}$$

---

$N = 3$, data: $(0,1), (1,-3), (2,7)$.

$$\sum x_i = 3, \quad \sum x_i^2 = 0+1+4 = 5$$
$$\sum y_i = 1-3+7 = 5, \quad \sum x_i y_i = 0(1)+1(-3)+2(7) = -3+14 = 11$$

$$\begin{cases} 3a + 3b = 5 \\ 3a + 5b = 11 \end{cases}$$

Subtract: $2b = 6 \implies b = 3$.

$3a = 5 - 9 = -4 \implies a = -4/3$.

$$\boxed{\varphi(x) = -\frac{4}{3} + 3x}$$

---

## Problem 2

> (2.5 points) Let $f \in C^2[-1, 1]$. Find a quadrature formula
> $$\int_{-1}^{1} |x| f(x)\,dx = A f(\alpha) + R(f),$$
> having the maximum degree of precision possible. Find an expression for the remainder $R(f)$.

**Formula used — method of undetermined coefficients:** 2 unknowns ($A$, $\alpha$), so max $d = 1$.

**$k=0$: $f(x) = 1$**

$$\int_{-1}^{1} |x| \cdot 1\,dx = A$$

$|x|$ is even, so $\int_{-1}^{1}|x|\,dx = 2\int_0^1 x\,dx = 2\cdot\frac{x^2}{2}\Big|_0^1 = 2\cdot\frac{1}{2} = 1$.

$$A = 1$$

**$k=1$: $f(x) = x$**

$$\int_{-1}^{1} |x| \cdot x\,dx = A \cdot \alpha$$

$|x| \cdot x = x|x|$ is an odd function (since $|{-x}|(-x) = -|x|x$), so $\int_{-1}^{1} x|x|\,dx = 0$.

$$1 \cdot \alpha = 0 \implies \alpha = 0$$

**Check $k=2$:**

$$\int_{-1}^{1} |x| \cdot x^2\,dx = 2\int_0^1 x^3\,dx = 2\cdot\frac{x^4}{4}\Bigg|_0^1 = 2\cdot\frac{1}{4} = \frac{1}{2}$$

$$A \cdot \alpha^2 = 1 \cdot 0 = 0 \neq \frac{1}{2}$$

So $d = 1$.

$$\boxed{\int_{-1}^{1} |x| f(x)\,dx = f(0) + R(f)}$$

**Remainder:** $R(e_2) = \frac{1}{2} - 0 = \frac{1}{2}$.

$$R(f) = \frac{1}{2!}f''(\xi) \cdot R(e_2) = \frac{1}{2}\cdot\frac{1}{2}\cdot f''(\xi) = \frac{1}{4}f''(\xi), \quad \xi \in (-1,1)$$

$$\boxed{R(f) = \frac{1}{4}f''(\xi), \quad \xi \in (-1,1)}$$

---

## Problem 3

> (2 points) Find the values of $c \in \mathbb{R}$ for which the iterative method $x_{n+1} = 2 - (1+c)x_n + cx_n^2$ converges to $\alpha = 1$ for a suitable $x_0$. Discuss the convergence of the method, the order of convergence and give a bound for the error.

**Formula used — one-point iteration $x_{n+1} = g(x_n)$:**

- Convergence: $|g'(\alpha)| < 1$.
- If $g'(\alpha) \neq 0$: order $p = 1$, rate $|g'(\alpha)|$.
- If $g'(\alpha) = 0$, $g''(\alpha) \neq 0$: order $p = 2$.
- Error bound: $|x_{n+1} - \alpha| \approx \frac{|g^{(p)}(\alpha)|}{p!}|x_n - \alpha|^p$.

---

$g(x) = 2 - (1+c)x + cx^2$. $g(1) = 2-(1+c)+c = 1$ $\checkmark$

$g'(x) = -(1+c) + 2cx$. $g'(1) = -(1+c)+2c = c-1$.

$|c-1| < 1 \iff \boxed{0 < c < 2}$

For $c \neq 1$: order $p = 1$, rate $|c-1|$, error: $|x_n-1| \leq |c-1|^n|x_0-1|$.

For $c = 1$: $g'(1)=0$, $g''(x) = 2c = 2 \neq 0$, order $p = 2$, error: $|x_{n+1}-1| \leq |x_n-1|^2$.

---

## Problem 4

> (2 points) Consider the system
> $$\begin{cases} 2x_1 + x_2 = -1 \\ x_1 + 2x_2 = -2 \end{cases}$$
> Compute the first three iterations in the Gauss-Seidel method, starting with $x^{(0)} = [0\ 0]^T$.

**Formula used — Gauss-Seidel iteration:** Like Jacobi, but use the **most recent** values:

$$x_i^{(k+1)} = \frac{1}{a_{ii}}\left(b_i - \sum_{j<i} a_{ij} x_j^{(k+1)} - \sum_{j>i} a_{ij} x_j^{(k)}\right)$$

---

$$x_1^{(k+1)} = \frac{1}{2}\left(-1 - x_2^{(k)}\right), \quad x_2^{(k+1)} = \frac{1}{2}\left(-2 - x_1^{(k+1)}\right)$$

Note: $x_2^{(k+1)}$ uses the **already updated** $x_1^{(k+1)}$.

**Iteration 1** ($k=0$, $x^{(0)} = [0, 0]^T$):

$$x_1^{(1)} = \frac{1}{2}(-1 - 0) = -\frac{1}{2}$$
$$x_2^{(1)} = \frac{1}{2}\left(-2 - \left(-\frac{1}{2}\right)\right) = \frac{1}{2}\left(-\frac{3}{2}\right) = -\frac{3}{4}$$

**Iteration 2** ($k=1$, $x^{(1)} = [-1/2, -3/4]^T$):

$$x_1^{(2)} = \frac{1}{2}\left(-1 - \left(-\frac{3}{4}\right)\right) = \frac{1}{2}\left(-\frac{1}{4}\right) = -\frac{1}{8}$$
$$x_2^{(2)} = \frac{1}{2}\left(-2 - \left(-\frac{1}{8}\right)\right) = \frac{1}{2}\left(-\frac{15}{8}\right) = -\frac{15}{16}$$

**Iteration 3** ($k=2$, $x^{(2)} = [-1/8, -15/16]^T$):

$$x_1^{(3)} = \frac{1}{2}\left(-1 - \left(-\frac{15}{16}\right)\right) = \frac{1}{2}\left(-\frac{1}{16}\right) = -\frac{1}{32}$$
$$x_2^{(3)} = \frac{1}{2}\left(-2 - \left(-\frac{1}{32}\right)\right) = \frac{1}{2}\left(-\frac{63}{32}\right) = -\frac{63}{64}$$

$$\boxed{x^{(3)} = \begin{bmatrix} -1/32 \\ -63/64 \end{bmatrix} \approx \begin{bmatrix} -0.03125 \\ -0.98438 \end{bmatrix}}$$

*Exact solution: $x = [0, -1]^T$. The Gauss-Seidel method converges faster than Jacobi.*
